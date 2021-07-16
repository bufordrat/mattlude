(* mattlude
 * mattlude.ml
 * Copyright (c) 2021 Matt Teichman. All rights reserved.
 * Distributed under the ISC license, see terms at the end of the file.
 * Mattlude Version 1.0
 *)

open Prelude
open Endofunctors

(** [version] is the library version metadata alist. *)
(* let version = V.data *)

module Free = struct
  module Make (F : FUNCTOR) = struct
    module FreeMonad = struct
      type 'a t =
        | Pure of 'a
        | Join of ('a t) F.t
      
      let pure x = Pure x
      
      let rec bind mx k = match mx with
        | Pure x -> k x
        | Join rest -> Join (F.map (flip bind @@ k) rest)
      
      let lift cmd = Join (F.map pure cmd)
    end
    include FreeMonad
    include Monad.ToApplicative (FreeMonad)
  end

  module Example = struct
    module Program = struct
      type 'next t =
        | Greeting of 'next
        | Prompt of (string -> 'next)
        | Message of string * 'next
        | Quit of 'next

      let map f = function
        | Greeting next -> Greeting (f next)
        | Prompt cont -> Prompt (fun x -> cont x |> f)
        | Message (msg, next) -> Message (msg, f next)
        | Quit next -> Quit (f next)
    end
    module FProg = Make (Program)

    let greeting = FProg.lift @@ Greeting ()
    let prompt = FProg.lift @@ Prompt id
    let message m = FProg.lift @@ Message (m, ())
    let quit = FProg.lift @@ Quit ()
 
    let repl : unit FProg.t =
      let open FProg in
      let one_round =
        let* () = message "Please type something!" in
        let* input = prompt in
        if input = "q"
        then quit
        else message (sprintf "You just typed %s!" input)
      in
      one_round
      
    let cool_program = FProg.(greeting >> repl)

    module DryRun = struct
      open FProg
      
      let rec dry_run = function
        | Pure next -> next
        | Join Greeting next ->
           print "This is where it would greet you" ;
           dry_run next
        | Join Prompt cont ->
           print "This is where you would type something in" ;
           cont "dummy value" |> dry_run
        | Join Message (_, next) ->
           print "This is where the program would say something to you" ;
           dry_run next
        | Join Quit _ -> ()
      
      let rec run = function
        | Pure next -> next
        | Join Greeting next ->
           print "Wzup!  Type 'q' to quit." ;
           run next
        | Join Prompt cont ->
           let input = input_line stdin in
           cont input |> run
        | Join Message (msg, next) ->
           print msg ;
           run next
        | Join Quit _ ->
           print "Bye!" ;
           exit 0
    end

    module Logger = struct

      module type RUN = sig
        include FUNCTOR
        (* 'a in the target type is side effect-ful *)
        val run : 'a t -> 'a
      end
      
      module Log = struct
        type 'next t =
          | Log of string * 'next
          | Silent of 'next
        
        let map f = function
          | Log (msg, next) -> Log (msg, f next)
          | Silent next -> Silent (f next)
      end
      
      module Interpreter = struct
        open Program

        type 'a t = 'a Program.t

        let run = function
          | Greeting next ->
             print "Wzup!  Type 'q' to quit." ;
             next
          | Prompt cont ->
             let input = input_line stdin in
             cont input
          | Message (msg, next) ->
             print msg ;
             next
          | Quit _ ->
             print "Bye!" ;
             exit 0

      end

      module LogInterpreter = struct
        module WL = Functor.Compose (Log) (Program)
        type 'a t = 'a WL.t
        let map = WL.map
        
        let run : 'a WL.t -> 'a = function
          | Log.Log (msg, next) -> print msg ; Interpreter.run next
          | Silent next -> Interpreter.run next
      end

      module RunFree (Intr : RUN) = struct
        module Rat = Make (Intr)

        let rec run_free =
          let open Rat in
          function
          | Pure x -> pure x
          | Join next -> Intr.run next |> run_free
      end

      module ComposeRuns (Intr1 : RUN) (Intr2 : RUN) = struct
        module F1 = Make (Intr1)
        module F2 = Make (Intr2)
        
        let rec augment : ('a Intr1.t -> 'a Intr2.t) -> 'a F1.t -> 'a F2.t =
          let open F1 in
          fun nt free ->
          match free with
          | Pure x -> Pure x
          | Join next -> Join (F1.map (augment nt) (nt next))
      end
      

      (* end *)

      let add_logs : 'a Interpreter.t -> 'a LogInterpreter.t=
        let open Program in
        let open Log in
        function
        | Greeting next ->
           Log ("LOG: displaying greeting",
                    Greeting next)
        | Prompt cont ->
           Log ("LOG: displaying prompt",
                Prompt cont)
        | Message (msg,next) ->
           Log ("LOG: printing message",
                Message (msg, next))
        | Quit next ->
           Log ("LOG: quitting",
                Quit next)

      (* module RunMain = RunFree (LogInterpreter)
       * 
       * let run_with_logging fr = Interpreter.run (RunMain.augment add_logs fr) *)
      
    end
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
  include Monad.ToApplicative (ParserMonad)

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
        String.(mem chr whitespace)
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

    let to_string = function
      | LParen -> "("
      | RParen -> ")"
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Div -> "/"
      | Num n -> sprintf "%i" n
      | Space -> " "
    
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
          let pop _ = assert false
          let re_append = append
      end

      (* module Lexer = Parser.Make (SeqTok) (SeqTok) *)

      (* let lexP =
       *   let open Lexer in
       *   let lparenP = pure LParen <* satisfy (eq '(') in
       *   let rparenP = pure RParen <* satisfy (eq ')') in
       *   let opP = 
       *     let is_op_chr chr = String.mem chr "+*/-" in
       *     let+ op_chr = satisfy is_op_chr
       *     in char_to_binop op_chr
       *   in
       *   let numP =
       *     let seq_to_int =
       *       int_of_string << String.implode << List.of_seq
       *     in
       *     let mk_num lst = Num (seq_to_int lst) in
       *     let+ numstring = munch1 Char.Decimal.is in
       *     mk_num numstring
       *   in
       *   let spaceP =
       *     let is_space chr = String.(mem chr whitespace) in
       *     pure Space <* munch1 is_space
       *   in
       *   choice [ lparenP; rparenP; opP; numP; spaceP ]
       * 
       * let lex str =
       *   match Lexer.many1 lexP str with
       *   | Ok (lst, f) -> begin
       *       match SeqTok.pop f with
       *       | None, _ -> Ok lst
       *       | _ -> Error "lexing error"
       *     end
       *   | Error e -> Error e *)

    end

    module Words = struct

      module ListTok = struct
        include List
        type tok = char
        type stream = tok t
        let empty = []
        let null = function
          | [] -> true
          | _ -> false
        let pop = function
          | [] -> None, []
          | x :: xs -> Some x, xs
        let re_append = append
      end

      
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
          let pop s = match s () with
            | Nil -> None, s
            | Cons (x, xs) -> begin
                (Some x, xs)
              end
          let re_append = append
      end

      (* module Lexer = Parser.Make (ListTok) (SeqTok)
       * 
       * let lexP =
       *   let open Lexer in
       *   let is_space chr =
       *     String.mem chr String.whitespace
       *   in
       *   sep_by1 (munch1 Char.Alphabetic.is) (munch1 is_space)
       *   
       * let lex str =
       *   match Lexer.many1 lexP str with
       *   | Ok (lst, f) -> begin
       *       match SeqTok.pop f with
       *       | None, _ -> Ok lst
       *       | _ -> Error "lexing error"
       *       end
       *   | Error e -> Error e
       * 
       * let of_chars chan =
       *   let eachchar _ = match input_char chan with
       *     | exception End_of_file -> None
       *     | char                  -> Some (char, chan)
       *   in
       *   Seq.unfold eachchar chan
       * 
       * let play fn f = within (of_chars >> f) fn *)

    end
                      
    open LexExample
  end

  module Parse = struct

    let pop = function
      | [] -> None, []
      | x :: xs -> Some x, xs
    
    module ListTok = struct
        include List
        type tok = Lex.lexeme
        type stream = tok t
        let empty = []
        let null = function
          | [] -> true
          | _ -> false
        let pop = pop
        let re_append = append
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

      let ops_ranking =
        List.map Parser.any_op [
            [(mk_eminus, "-")] ;
            [(mk_eplus, "+")] ;
            [(mk_ediv, "/")] ;
            [(mk_etimes, "*")] ;
          ]
      
      let skip_spaces = Parser.skip_spaces

      let numP =
        let open Parser in
        let mk_num n = Num n
        in let+ c = satisfy Char.Decimal.is
           in mk_num @@ (int_of_string << String.make 1) c

      let enumP = Parser.map mk_numexp numP
           
      let (expr,_) =
        Parser.mk_expr enumP ops_ranking "(" ")"
      
    end
    
    module P = Parser.Make (ListTok) (ListTok)

    (* let ops_ranking =
     *   List.map P.any_op [
     *       [(mk_eminus, [Lex.Minus])] ;
     *       [(mk_eplus, [Lex.Plus])] ;
     *       [(mk_ediv, [Lex.Div])] ;
     *       [(mk_etimes, [Lex.Times])] ;
     *     ] *)
    
    (* let skip_spaces =
     *   let open P in
     *   optional (satisfy Lex.is_space)
     *               
     * let enumP =
     *   let open P in
     *   let mk_num = function
     *     | Lex.Num n -> Num_exp (Num n)
     *     | _ -> assert false
     *   in
     *   let+ lexeme = satisfy Lex.is_num
     *   in mk_num lexeme
     * 
     * let (expP,_) =
     *   let open P in
     *   let open Lex in
     *   mk_expr enumP ops_ranking [LParen] [RParen] *)
    
    (* let rec binopP input =
     *   let open P in
     *   let open Lex in
     *   let op prsr pred =
     *       pure prsr
     *       <* (satisfy is_lparen)
     *       <* skip_spaces
     *       <*> expP
     *       <* skip_spaces
     *       <* (satisfy pred)
     *       <* skip_spaces
     *       <*> expP
     *       <* skip_spaces
     *       <* (satisfy is_rparen)
     *   in
     *   choice [
     *       op mk_plus is_plus ;
     *       op mk_minus is_minus ;
     *       op mk_times is_times ;
     *       op mk_div is_div ;
     *     ] @@ input
     *   
     * and expP input =
     *   let open P in
     *   choice [
     *       pure mk_numexp <*> numP ;
     *       pure mk_opexp <*> binopP ;
     *     ] @@ input *)
  end
end

module type PEEKABLE = sig
  type 'a peekaboo
  val peek : 'a peekaboo -> 'a
end

module LetsPeek : PEEKABLE = struct
  type 'a peekaboo = 'a List.t
  let peek = List.hd
end


module type COLLECTION = sig
  type elt
  val get_head : elt List.t -> elt
end

module LetsCollect : COLLECTION = struct
  type elt = int
  let get_head = List.hd
end


(* module type ADDABLE = sig
 *   type number
 *   val plus : number -> number -> number
 * end *)

(* module type ADDABLE_INT = sig
 *   include ADDABLE with type number = int
 * end
 * 
 * module CoolNumber : ADDABLE with type number = int = struct
 *   type number = int
 *   let plus = (+)
 * end *)

module type ADDABLE = sig
  type number
  val plus : number -> number -> number
end

module type RING = sig
  include ADDABLE
  val times : number -> number -> number
end

module ExampleRing : RING = struct
  type number = int
  let plus = (+)
  let times = ( * )
end

type small = [ `A | `B ]

type big = [ small | `C ]

let example1 : small = `A

let example2 : small :> big = example1

let example3 :> big = example1

let example4 = (example1 :> big)

type keith_or_matt = Keith | Matt

type keith_or_matt_or_whatever =
  [ `Keith | `Matt ]



(* let () = FreeExample.(run cool_program) *)


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
