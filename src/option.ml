include Prelude.Option
include Stdlib.Option

open Endofunctors

module OptionMonad = struct
  type 'a t = 'a Stdlib.Option.t
  let pure = some
  let bind = bind
end

include Monad.Make (OptionMonad)

let cat_options lst =
  let rec cat_options' acc = function
    | [] -> acc
    | Some x :: xs -> cat_options' (x :: acc) xs
    | None :: xs -> cat_options' acc xs
  in
  Stdlib.List.rev @@ cat_options' [] lst
