include Prelude.List
include Stdlib.List

module Assoc = Prelude.Assoc

open Endofunctors

module ListMonad = struct
  type 'a t = 'a Stdlib.List.t
  let pure x = [x]
  let bind = Prelude.List.bind
end

module M = Monad.Make (ListMonad)
include M

module Traverse = struct
  module T = Traverse.List.Make (M)
  let sequence = T.sequence
  let forM = T.forM
  let traverse = T.traverse
end
include Traverse
