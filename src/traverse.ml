module type STREAM = 
  Traverse_intf.Traversable.STREAM

module type IDIOM =
  Traverse_intf.Traversable.IDIOM

module Make (S : STREAM) (I : IDIOM) = struct
  open Endofunctors

  module Idiom = Applicative.Make (I)
  module Stream = Functor.Make (S)

end
