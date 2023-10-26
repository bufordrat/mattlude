module type STREAM = 
  Traverse_intf.Traversable.STREAM

module type IDIOM =
  Traverse_intf.Traversable.IDIOM

module Make (S : STREAM) (I : IDIOM) = struct
  open Endofunctors

  module Idiom = Applicative.Make (I)
  module Stream = Functor.Make (S)
end

module List = struct
  module Make (I : IDIOM) = struct
    open Endofunctors
    include Applicative.Make (I)
    let sequence lst =
      let reducer acc ax =
        let+ x = ax
        and+ xs = acc
        in x :: xs
      in
      let open Prelude in
      foldl reducer (I.pure []) lst >>| rev
  end
end
