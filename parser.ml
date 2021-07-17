(* IS short for 'input stream'
 * OS short for 'output stream' *)

open Prelude
open Endofunctors

module type TOKEN = sig
  include FOLDABLE
  type tok
  type stream = tok t
  val pop : 'a t -> 'a option * 'a t
  val cons : 'a -> 'a t -> 'a t
  val re_append : 'a t -> 'a t -> 'a t
  val rev: 'a t -> 'a t
end

module Make (OS: TOKEN) (IS : TOKEN) = struct

  module PResult = Result.Make (String)

  module ParseResult = struct
    type ('a, 'b) t = { result : ('a, 'b) PResult.t ;
                        consumed : IS.tok IS.t
                      }
  end

  module ParserMonad = struct
    type 'output t =
      IS.stream -> (('output * IS.stream), string) result
    let pure x = fun stream -> PResult.ok (x, stream)
    let bind prsr k = let open PResult in 
                      fun input ->
                      let* (result1, remainder1) = prsr input in
                      (k result1) remainder1
  end
  include Monad.ToApplicative (ParserMonad)
  
  let run_parser prsr input = prsr input
  
  module Combinators = struct

    let alternative prsr1 prsr2 input =
      match prsr1 input with
      | Error _ -> prsr2 input
      | _ -> prsr1 input
    
    let (<|>) = alternative
    
    let succeed input = PResult.ok input

    let fail _ = PResult.error "error: pfail"

    let choice prsrs = List.foldl (<|>) fail prsrs

    let optional prsr = prsr *> pure () <|> pure ()
    
    let satisfy pred input =
      match IS.pop input with
      | None, _ -> PResult.error "end of file"
      | Some x, xs -> begin
          if pred x
          then PResult.ok (x, xs)
          else fail (IS.cons x xs)
        end
    
    let eof input =
      match IS.pop input with
      | None, _ -> PResult.ok ((), IS.empty)
      | Some x, xs -> fail (IS.cons x xs)

    let rec many prsr input =
      match prsr input with
      | Ok _ -> (pure OS.cons <*> prsr <*> many prsr) input
      | Error _ -> (pure OS.empty) input

    let many1 prsr = pure OS.cons <*> prsr <*> many prsr

    let sep_by1 prsr sepPrsr =
      let+ initial = many (prsr <* sepPrsr)
      and+ final = prsr
      in
      OS.(append initial @@ cons final empty)

    let munch1 pred = many1 (satisfy pred)

    let token tok = satisfy (eq tok)

    let tokens toks = 
      let concat_tok toksP tok =
        let+ toks = toksP
        and+ token = token tok in
        OS.(append toks @@ cons token empty)
      in
      OS.(foldl concat_tok @@ pure empty) toks

    let chainl op p =
      let rec apply_all x stream =
        match OS.pop stream with
        | None, _ -> x
        | Some f, fs -> apply_all (f x) (fs)
      in
      apply_all <$> p <*> many (flip <$> op <*> p)
    
    let any_op input =
      let op (func, toks) = pure func <* tokens toks in
      (choice << List.map op) input
    
    let mk_expr atomic ranking left right =
      let pack l prsr r = tokens l *> prsr <* tokens r in
      let rec expr input = (foldr chainl factor ranking) input
      and factor input = (atomic <|> pack left expr right) input
      in (expr, factor)
  end
  include Combinators
end
