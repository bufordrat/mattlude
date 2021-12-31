open Prelude
open Endofunctors

module StringParser = struct

  module Error = struct
    type t = {
        msg : string ;
        remainder : string ;
      }
  end
               
  module PResult = Result.Make (Error)

  module ParserMonad = struct
    type 'output t =
      string -> (('output, string) result * string)
    let pure x input = (PResult.ok x, input)
    let bind prsr k input =
      let open PResult in 
      let (mresult1, remainder1) = prsr input in
      match mresult1 with
      | Ok value -> (k value) remainder1
      | Error _ as err -> (err, remainder1)
  end
  include Monad.ToApplicative (ParserMonad)

  let alternative prsr1 prsr2 input =
    match prsr1 input with
    | Error _, _ -> prsr2 input
    | _ -> prsr1 input
  let (<|>) = alternative

  (* let consumes_input prsr input
   *   = snd (prsr input) <> input *)
            
  module Combinators = struct

    let succeed input = PResult.ok input

    let fail msg remainder =
      (PResult.error msg, remainder) 

    let choice prsrs = List.foldl (<|>) (fail "choice") prsrs

    let optional prsr = prsr *> pure () <|> pure ()
                     
    let satisfy ?(msg_override = "") pred =
      let open String in
      let compute_override ?(got = "EOF") msg =
        if msg = ""
        then sprintf "satisfy: got: %s" got
        else sprintf "expected: %s got: %s" msg got
      in
      function
      | "" ->
         let msg =
           compute_override msg_override
         in
         let remainder = "" in
         (PResult.error msg, remainder)
      | str ->
         let head = str.[0] in
         let got = sub str 0 1 in
         let tail = sub str 1 (length str - 1) in
         let msg =
           compute_override ~got:got msg_override
         in
         let remainder = str in
         if pred head
         then (PResult.ok head, tail)
         else (PResult.error msg, remainder)

    let eof = function
      | "" -> (PResult.ok (), "")
      | other ->
         let msg =
           sprintf "expected: EOF got: %c" other.[0]
         in
         let remainder = other in
         (PResult.error msg , remainder)
    
    let char c =
      let msg = String.make 1 c in
      satisfy ~msg_override:msg (fun x -> x = c)

    let string str = 
      let concat_char strP chr =
        let+ str = strP
        and+ chr = char chr in
        str ^ String.make 1 chr
      in
      String.foldl concat_char (pure "") str

    let rec many prsr input =
      match prsr input with
      | Ok _, _ -> (pure cons <*> prsr <*> many prsr) input
      | Error _, _ -> (pure []) input

    let many1 prsr = pure cons <*> prsr <*> many prsr

    let negate_parser prsr msg input =
      match prsr input with
      | Error _, _ -> pure () @@ input
      | _ -> fail msg @@ input

    let sep_by1 prsr sep =
      let msg = "error: too many separators" in
      let a_bunch = 
        let+ initials = many1 (prsr <* sep)
        and+ final = prsr in
        initials @ [final]
      in
      let just_one =
        let+ final = prsr <* negate_parser sep msg in
        [final]
      in
      a_bunch <|> just_one

    let is_space chr =
      String.(mem chr whitespace)
       
    let spaces = many1 @@ satisfy @@ is_space

    let spaces1 = satisfy is_space <* spaces

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

    module RunParser = struct
      let compute_lines size str =
        let open String in
        let fragment = take size str in
        let reducer count chr =
          if chr == '\n'
          then count + 1
          else count
        in
        foldl reducer 1 fragment
      
      let parse_string prsr str =
        match prsr str with
        | Ok output, _ -> PResult.ok output
        | Error e, remainder ->
           let offset = String.(length str - length remainder) in
           let line = compute_lines offset str in
           let line_msg = sprintf "Line: %i " line in
           let col_msg = sprintf "Column: %i " (offset + 1) in
           PResult.error (line_msg ^ col_msg ^ e)
    end
    include RunParser
  end
  include Combinators
end

module TokenParser = struct                    
  module type TOKEN = sig
    include FOLDABLE
    type tok
    type stream = tok t
    val pop : 'a t -> 'a option * 'a t
    val cons : 'a -> 'a t -> 'a t
    val re_append : 'a t -> 'a t -> 'a t
    val rev: 'a t -> 'a t
  end

  (* IS short for 'input stream'
   * OS short for 'output stream' *)
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
end
