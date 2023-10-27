module List : sig
  type 'a t = 'a list
  module Make :
  functor (I : Traverse_intf.Traversable.IDIOM) ->
  sig
    include Traverse_intf.Traversable.List.AUGMENTED with type 'a t := 'a I.t 
  end
end
