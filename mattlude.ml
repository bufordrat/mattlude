(* mattlude
 * mattlude.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * Mattlude Version 1.0
 *)

open Prelude

(** [version] is the library version metadata alist. *)
let version = V.data

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

module type ERROR = sig
  type t
end
                
module Result = struct
  (* module functor for building a Result module with cool extra stuff
     in it; takes a module containing the error type as an input *) 
  module Make (E : ERROR) = struct
    include Result
    
    (* for auto-generating monad and applicative stuff *)
    module ResultMonad = struct
      type 'a t = ('a, E.t) result
      let pure = Result.ok
      let bind = Result.(>>=)
    end
    
    include Monad2App (ResultMonad)
  end
end

module type MONOID = sig
  type 'a t
  val empty : 'a t
  val append : 'a t -> 'a t -> 'a t
end

module type FOLDABLE = sig
  include MONOID
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val null : 'a t -> bool
end

module type TOKEN = sig
  include FOLDABLE
  type tok
  type stream = tok t
  val hd : 'tok t -> 'tok
  val tl : 'tok t -> 'tok t
  val cons : 'tok -> 'tok t -> 'tok t
end


module Parser = struct
  (* IS short for 'input stream'
   * OS short for 'output stream' *)
  module Make (OS: TOKEN) (IS : TOKEN) = struct
    module PResult = Result.Make (String)

    module ParserMonad = struct
      type 'output t =
        IS.stream -> (('output * IS.stream), string) result
      let pure x = fun stream -> PResult.ok (x, stream)
      let bind prsr k = let open PResult in 
                        fun input ->
                        let* (result1, remainder1) = prsr input in
                        (k result1) remainder1
    end
    include Monad2App (ParserMonad)
    
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
      
      let satisfy pred = function
        | r when IS.null r -> PResult.error "end of file"
        | toks -> begin
            if pred (IS.hd toks)
            then PResult.ok (IS.hd toks, IS.tl toks)
            else PResult.error "error: satisfy"
          end
      
      let eof = function
        | r when r = IS.empty -> PResult.ok ((), IS.empty)
        | _ -> PResult.error "error: eof"

      let token tok = satisfy (fun x -> x = tok)

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
        let rec apply_all x = function
          | s when OS.null s -> x
          | stream -> apply_all ((OS.hd stream) x) (OS.tl stream)
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
end


module StringParserF = struct
  module PResult = Result.Make (String)

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
            
  module Combinators = struct

    let succeed input = PResult.ok input

    let fail _ = PResult.error "error: pfail"

    let choice prsrs = List.foldl (<|>) fail prsrs

    let optional prsr = prsr *> pure () <|> pure ()
                     
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
           then head ^ fst recurse, snd recurse
           else "", str
      in
      match span pred input with
      | ("",_) -> PResult.error "error: span"
      | _ -> PResult.ok (span pred input)

    let eof = function
      | "" -> PResult.ok ((), "")
      | _ -> PResult.error "error: eof"
    
    let char c = satisfy (fun x -> x = c)

    let string str = 
      let concat_char strP chr =
        let+ str = strP
        and+ chr = char chr in
        str ^ String.make 1 chr
      in
      String.foldl concat_char (pure "") str
               
    let parse_string prsr str =
      match prsr str with
      | Ok (output, []) -> Ok output
      | Error _ as e -> e
      | _ -> Error "partial parse"
                
    let rec many prsr input =
      match prsr input with
      | Ok _ -> (pure cons <*> prsr <*> many prsr) input
      | Error _ -> (pure []) input

    let many1 prsr = pure cons <*> prsr <*> many prsr

    let sep_by1 prsr sepPrsr =
      let+ initial = many (prsr <* sepPrsr)
      and+ final = prsr
      in initial @ [final]

    let skip_spaces1 =
      let is_space chr =
        String.mem chr "\r\n\t "
      in
      pure () <* munch1 is_space

    let skip_spaces = skip_spaces1 <|> pure ()

    let rec sequence = function
      | [] -> pure []
      | x :: xs -> let+ p1 = x
                   and+ p2 = sequence xs
                   in cons p1 p2
    
    let chainl op p =
      let rec apply_all x = function
        | [] -> x
        | f :: fs -> apply_all (f x) fs
      in
      apply_all <$> p <*> many (flip <$> op <*> p)
   
    let pack l prsr r = string l *> prsr <* string r
    
    let op (func, str) = pure func <* string str

    let any_op input = (choice << List.map op) input
      
    let mk_expr atomic ranking left right =
      let rec expr input = (foldr chainl factor ranking) input
      and factor input = (atomic <|> pack left expr right) input
      in (expr, factor)

    (* let rec expr input = (foldr chainl factor ops_ranking) input
     * and factor input = (integer <|> pack "(" expr ")") input *)

    (* let rec expr input = (foldr chainl factor [addops; mulops]) input
     * and factor input = (integer <|> pack "(" expr ")") input *)
    
    (* let plus_minus =
     *   chainl
     *     (pure (-) <* char '-' <|> pure (+) <* char '+')
     *     integer *)
      
  end
  include Combinators
end

                     
module Example = struct

  module Lex = struct         
    type lexeme =
      | LParen
      | RParen
      | Plus
      | Minus
      | Times
      | Div
      | Num of int
      | Space

    let char_to_binop = function
      | '+' -> Plus
      | '-' -> Minus
      | '*' -> Times
      | '/' -> Div
      | _ -> assert false

    let is_plus = function Plus -> true | _ -> false 
    let is_minus = function Minus -> true | _ -> false 
    let is_times = function Times -> true | _ -> false 
    let is_div = function Div -> true | _ -> false 
    let is_lparen = function LParen -> true | _ -> false
    let is_rparen = function RParen -> true | _ -> false
    let is_space = function Space -> true | _ -> false
    let is_num = function Num _ -> true | _ -> false

    module LexExample = struct
      module Lexer = StringParserF 
                    
      let lexP =
        let open Lexer in
        let lparenP = pure LParen <* satisfy (eq '(') in
        let rparenP = pure RParen <* satisfy (eq ')') in
        let opP = 
          let is_op_chr chr = String.mem chr "+*/-" in
          let+ op_chr = satisfy is_op_chr
          in char_to_binop op_chr
        in
        let numP =
          let mk_num str = Num (int_of_string str) in
          let+ numstring = munch1 (Char.Decimal.is)
          in mk_num numstring
        in
        let spaceP = pure Space <* skip_spaces1 in
        choice [ lparenP; rparenP; opP; numP; spaceP ]
        
      let lex str =
        match Lexer.many1 lexP str with
        | Ok (lst, "") -> Ok lst
        | Ok (_, _) -> Error "lexing error"
        | Error e -> Error e
    end

    module SeqExample = struct
      module SeqTok = struct
          include Seq
          type tok = char
          type stream = tok t
          let null s =
            match s () with
            | Seq.Nil -> true
            | Seq.Cons _ -> false
      end

      module Lexer = Parser.Make (SeqTok) (SeqTok)

      let lexP =
        let open Lexer in
        let lparenP = pure LParen <* satisfy (eq '(') in
        let rparenP = pure RParen <* satisfy (eq ')') in
        let opP = 
          let is_op_chr chr = String.mem chr "+*/-" in
          let+ op_chr = satisfy is_op_chr
          in char_to_binop op_chr
        in
        let numP =
          let seq_to_int =
            int_of_string << String.implode << List.of_seq
          in
          let mk_num lst = Num (seq_to_int lst) in
          let+ numstring = munch1 Char.Decimal.is in
          mk_num numstring
        in
        let spaceP =
          let is_space chr = String.(mem chr whitespace) in
          pure Space <* munch1 is_space
        in
        choice [ lparenP; rparenP; opP; numP; spaceP ]

      let lex str =
        match Lexer.many1 lexP str with
        | Ok (lst, f) -> if SeqTok.null f
                         then Ok lst
                         else Error "lexing error"
        | Error e -> Error e
    end

    module Words = struct

      module SeqTok = struct
          include Seq
          type tok = char
          type stream = tok t
          let hd s =
            match s () with
            | Seq.Cons (x,_) -> x
            | Seq.Nil -> assert false
          let null s =
            match s () with
            | Seq.Nil -> true
            | Seq.Cons _ -> false
      end

      module Lexer = Parser.Make (SeqTok) (SeqTok)

      let lexP =
        let open Lexer in
        let is_space chr =
          String.mem chr String.whitespace
        in
        sep_by1 (munch1 Char.Alphabetic.is) (munch1 is_space)
        
      let lex str =
        match Lexer.many1 lexP str with
        | Ok (lst, f) -> if SeqTok.null f
                         then Ok lst
                         else Error "lexing error"
        | Error e -> Error e

      let of_chars chan =
        let eachchar _ = match input_char chan with
          | exception End_of_file -> None
          | char                  -> Some (char, chan)
        in
        Seq.unfold eachchar chan
      
      let play fn f = within (of_chars >> f) fn

    end
                      
    open LexExample
  end

  module Parse = struct
  
    module ListTok = struct
        include List
        type tok = Lex.lexeme
        type stream = tok t
        let empty = []
        let null = function
          | [] -> true
          | _ -> false
      end
  
    module SeqTok = struct
        include Seq
        type tok = Lex.lexeme
        type stream = tok t
        let null s =
          match s () with
          | Seq.Nil -> true
          | Seq.Cons _ -> false
      end
      
    type num = Num of int
      
    type binop =
      | Plus of (exp * exp)
      | Minus of (exp * exp)
      | Times of (exp * exp)
      | Div of (exp * exp)
  
    and exp =
      | Num_exp of num
      | Op_exp of binop
  
    let mk_plus exp1 exp2 = Plus (exp1, exp2)
    let mk_minus exp1 exp2 = Minus (exp1, exp2)
    let mk_times exp1 exp2 = Times (exp1, exp2)
    let mk_div exp1 exp2 = Div (exp1, exp2)

    let mk_eplus exp1 exp2 = Op_exp (mk_plus exp1 exp2)
    let mk_eminus exp1 exp2 = Op_exp (mk_minus exp1 exp2)
    let mk_etimes exp1 exp2 = Op_exp (mk_times exp1 exp2)
    let mk_ediv exp1 exp2 = Op_exp (mk_div exp1 exp2)

    let mk_numexp n = Num_exp n
    let mk_opexp o = Op_exp o

    module StringExample = struct
      module Parser = StringParserF

      let skip_spaces = Parser.skip_spaces

      let numP =
        let open Parser in
        let mk_num n = Num n
        in let+ c = satisfy Char.Decimal.is
           in mk_num @@ (int_of_string << String.make 1) c

      let enumP = Parser.map mk_numexp numP
      
      let ops_ranking =
        List.map Parser.any_op [
            [(mk_eminus, "-")] ;
            [(mk_eplus, "+")] ;
            [(mk_ediv, "/")] ;
            [(mk_etimes, "*")] ;
          ]
      
      let (expr,_) =
        Parser.mk_expr enumP ops_ranking "(" ")"
      
    end
    
    module P = Parser.Make (SeqTok) (SeqTok)
  
    let skip_spaces =
      let open P in
      optional (satisfy Lex.is_space)
                  
    let numP =
      let open P in
      let mk_num = function
        | Lex.Num n -> Num n
        | _ -> assert false
      in
      let+ lexeme = satisfy Lex.is_num
      in mk_num lexeme
       
    let rec binopP input =
      let open P in
      let open Lex in
      let op prsr pred =
          pure prsr
          <* (satisfy is_lparen)
          <* skip_spaces
          <*> expP
          <* skip_spaces
          <* (satisfy pred)
          <* skip_spaces
          <*> expP
          <* skip_spaces
          <* (satisfy is_rparen)
      in
      choice [
          op mk_plus is_plus ;
          op mk_minus is_minus ;
          op mk_times is_times ;
          op mk_div is_div ;
        ] @@ input
      
    and expP input =
      let open P in
      choice [
          pure mk_numexp <*> numP ;
          pure mk_opexp <*> binopP ;
        ] @@ input
  end
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
