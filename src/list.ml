include Stdlib.List

open Endofunctors

module ListMonad = struct
  type 'a t = 'a Stdlib.List.t
  let pure x = [x]
  let bind = Prelude.List.bind
end

module M = Monad.Make (ListMonad)
include M

module T = Traverse.List.Make (M)
let sequence = T.sequence
