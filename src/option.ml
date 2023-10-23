include Prelude.Option
include Stdlib.Option

module Monad = Endofunctors.Monad 

let cat_options lst =
  let rec cat_options' acc = function
    | [] -> acc
    | Some x :: xs -> cat_options' (x :: acc) xs
    | None :: xs -> cat_options' acc xs
  in
  List.rev @@ cat_options' [] lst

module OptionMonad = struct
  type 'a t = 'a Stdlib.Option.t
  let pure = some
  let bind = bind
end

include Monad.Make (OptionMonad)
