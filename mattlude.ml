(* mattlude
 * mattlude.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * Mattlude Version 1.0
 *)

open Prelude

(** [version] is the library version metadata alist. *)
(* let version = V.data *)

module type FUNCTOR = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end
   
module type APPLICATIVE = sig
  include FUNCTOR
  val product : 'a t -> 'b t -> ('a * 'b) t
end

module type MONAD = sig
  type 'a t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

                 
module FunctorGoodies (F : FUNCTOR) = struct
  let (let+) x f = F.map f x
  let (>>|) = (let+)
  let (<&>) = (let+)
  let (>|=) = (let+)
  let (<$>) = F.map
end
                  
module ApplicativeGoodies (A : APPLICATIVE) = struct
  include FunctorGoodies (A)
  let (and+) = A.product
  let apply af ax =
    A.map
      (fun (f, x) -> f x)
      (A.product af ax)
  let (<*>) = apply
end

module MonadGoodies (M : MONAD) = struct
  let pure = M.pure
  let bind = M.bind
  let (>>=) = bind
  let (let*) = bind
  let (>=>) mf mg x = mf x >>= mg
  let (<=<) mf mg x = mg x >>= mf
  let (>>) mx my = mx >>= fun _ -> my
end
                               
module Monad2App (M : MONAD) = struct
  include MonadGoodies (M)

  (* reduction of applicative interface to monadic interface *)
  let map f mx = let* x = mx in
                 pure (f x)
  let product ax ay = let* x = ax in
                      let* y = ay in
                      pure (x,y)

  module I = struct
    type 'a t = 'a M.t
    let map = map
    let product = product
  end

  include ApplicativeGoodies (I)
  let ( <* ) ax ay = pure k <*> ax <*> ay
  let ( *> ) ax ay = pure (flip k) <*> ax <*> ay
end

(* helper functions for optional values *)
module Option = struct

  (* unwraps the Somes; throws the None-s out *)
  include Option
  let rec cat_options = function
    | [] -> []
    | Some x :: xs -> x :: cat_options xs
    | None :: xs -> cat_options xs

  (* for auto-generating monad and applicative stuff *)
  module OptionMonad = struct
    type 'a t = 'a option
    let pure = Option.some
    let bind = Option.(>>=)
  end

  include Monad2App (OptionMonad)
end
let cat_options = Option.cat_options

module type ERROR = sig
  type t
end
                
(* module functor for building a Result module with cool extra stuff in it;
   takes a module containing the error type as an input *)
module ResultF (E : ERROR) = struct
  include Result

  (* for auto-generating monad and applicative stuff *)
  module ResultMonad = struct
    type 'a t = ('a, E.t) result
    let pure = Result.ok
    let bind = Result.(>>=)
  end

  include Monad2App (ResultMonad)
end

module type MONOID = sig
  type 'a t
  val empty : 'a t
  val (++) : 'a t -> 'a t -> 'a t
end
                           
module type TOKEN = sig
  include MONOID
  type tok
  type stream = tok t
  val end_insert : tok -> tok t -> tok t
end
                           
module ParserF (T : TOKEN) (E : ERROR) = struct
  module PResult = ResultF (E)

  module ParserMonad = struct
    type 'output t =
      T.stream -> (('output * T.stream), E.t) result
    let pure x = fun stream -> PResult.ok (x, stream)
    let bind prsr k = let open PResult in 
                      fun input ->
                      let* (result1, remainder1) = prsr input in
                      (k result1) remainder1
  end
  include Monad2App (ParserMonad)

  let alternative prsr1 prsr2 input =
    match prsr1 input with
    | Error _ -> prsr2 input
    | _ -> prsr1 input
  let (<|>) = alternative

  let run_parser prsr input = prsr input
            
  module KleisliArrows = struct
    let satisfy pred = function
      | [] -> PResult.error "end of file"
      | tok :: toks -> if pred tok
                       then PResult.ok (tok, toks)
                       else PResult.error "error: satisfy"

    let munch1 pred input =
      let rec span pred = function
        | [] -> ([], [])
        | x :: xs as lst -> 
           if pred x
           then x :: fst (span pred xs), snd (span pred xs)
           else [], lst
      in
      match span pred input with
      | ([],_) -> PResult.error "error: span"
      | _ -> PResult.ok (span pred input)

    let eof = function
      | [] -> PResult.ok ((), [])
      | _ -> PResult.error "error: eof"

    let token tok = satisfy (fun x -> x = tok)
                     
  end
  include KleisliArrows
end


module StringParserF = struct
  module PResult = ResultF (String)

  module ParserMonad = struct
    type 'output t =
      string -> (('output * string), string) result
    let pure x = fun stream -> PResult.ok (x, stream)
    let bind prsr k = let open PResult in 
                      fun input ->
                      let* (result1, remainder1) = prsr input in
                      (k result1) remainder1
  end
  include Monad2App (ParserMonad)

  let alternative prsr1 prsr2 input =
    match prsr1 input with
    | Error _ -> prsr2 input
    | _ -> prsr1 input
  let (<|>) = alternative
            
  module KleisliArrows = struct
    let satisfy pred = let open String in function
      | "" -> PResult.error "end of file"
      | str ->
         let head = str.[0] in
         let tail = sub str 1 (length str - 1) in
         if pred head
         then PResult.ok (head, tail)
         else PResult.error "error: satisfy"

    let munch1 pred input =
      let open String in
      let rec span pred = function
        | "" -> ("", "")
        | str ->
           let head = sub str 0 1 in
           let recurse = sub str 1 (length str - 1) |> span pred in
           if pred str.[0]
           then 
                head ^ fst recurse, snd recurse
           else "", str
      in
      match span pred input with
      | ("",_) -> PResult.error "error: span"
      | _ -> PResult.ok (span pred input)

    let eof = function
      | "" -> PResult.ok ((), "")
      | _ -> PResult.error "error: eof"
    
    let char c = satisfy (fun x -> x = c)

    let parse_string prsr str =
      match prsr str with
      | Ok (output, []) -> Ok output
      | Error _ as e -> e
      | _ -> Error "partial parse"

    let succeed input = PResult.ok input

    let fail _ = PResult.error "error: pfail"
                
    let string str = 
      let concat_char strP chr =
        let+ str = strP
        and+ chr = char chr in
        str ^ String.make 1 chr
      in
      String.foldl concat_char (pure "") str

    let rec many prsr input =
      match prsr input with
      | Ok _ -> (pure cons <*> prsr <*> many prsr) input
      | Error _ -> (pure []) input

    let many1 prsr = pure cons <*> prsr <*> many prsr

  end
  include KleisliArrows
end


                

(*
 * Copyright (c) 2021 Matt Teichman
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
