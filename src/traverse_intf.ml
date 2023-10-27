module Semigroup = struct
  module type BASIC = sig
    type 'a t
    val append : 'a t -> 'a t -> 'a t
  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    val (<|>) : 'a t -> 'a t -> 'a t
  end
end
module type SEMIGROUP = Semigroup.BASIC

module Monoid = struct
  module type BASIC = sig
    type 'a t
    include Semigroup.BASIC with type 'a t := 'a t
    val empty : 'a t
  end

  module type AUGMENTED = sig
    type 'a t
    include Semigroup.AUGMENTED with type 'a t := 'a t
    include BASIC with type 'a t := 'a t
    val sum : 'a t list -> 'a t
  end
end
module type MONOID = Monoid.BASIC

module Foldable = struct
  module type BASIC = sig
    type 'a t
    include MONOID with type 'a t := 'a t
    val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    val null : 'a t -> bool
  end
end
module type FOLDABLE = Foldable.BASIC

module Traversable = struct
  open Endofunctors_intf

  module type STREAM = sig
    type 'a t
    include Functor.BASIC with type 'a t := 'a t
    include Foldable.BASIC with type 'a t := 'a t
  end
  
  module type IDIOM = sig
    type 'a t
    include Applicative.BASIC with type 'a t := 'a t
  end

  (* TODO: abstract over container datatypes other than lists; Haskell
     handwaves this a bit *)

  module List = struct
    module type BASIC = sig
      type 'a t 
      val sequence : 'a t list -> 'a list t
    end

    module type AUGMENTED = sig
      type 'a t
      include BASIC with type 'a t := 'a t
      val traverse : ('a -> 'b t) -> 'a list -> 'b list t
      val forM : 'a list -> ('a -> 'b t) -> 'b list t
    end
  end
end
