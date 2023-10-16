module type SEMIGROUP = sig
  type 'a t
  val append : 'a t -> 'a t -> 'a t
end

module type MONOID = sig
  type 'a t
  include SEMIGROUP with type 'a t := 'a t
  val empty : 'a t
end

module type FOLDABLE = sig
  type 'a t
  include MONOID with type 'a t := 'a t
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val null : 'a t -> bool
end

module Functor = struct
  module type BASIC = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val (<&>) : 'a t -> ('a -> 'b) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  end

  module Augmented (F : BASIC) = struct
    include F 
    let ( let+ ) x f = map f x
    let (>>|) = ( let+ )
    let (<&>) = ( let+ )
    let (>|=) = ( let+ )
    let (<$>) = map
  end
  module _ : functor (F : BASIC) -> AUGMENTED = Augmented

  module Compose (F1 : BASIC) (F2 : BASIC) = struct
    type 'a t = 'a F2.t F1.t
    let map f composed = F1.map (F2.map f) composed
  end
  module _ : functor (F1 : BASIC) (F2 : BASIC) -> BASIC = Compose

end
module type FUNCTOR = Functor.AUGMENTED

module type TRAVERSABLE = sig
  type 'a t
  include FUNCTOR with type 'a t := 'a t
  include FOLDABLE with type 'a t := 'a t
end

module Applicative = struct
  module type BASIC = sig
    type 'a t
    include FUNCTOR with type 'a t := 'a t
    val product : 'a t -> 'b t -> ('a * 'b) t
  end

  module type AUGMENTED = sig
    type 'a t
    include Functor.AUGMENTED with type 'a t := 'a t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end

  module Augmented (A : BASIC) = struct
    include Functor.Augmented (A)
    type 'a t = 'a A.t
    let ( and+ ) = A.product
    let apply af ax = A.map
                        (fun (f, x) -> f x)
                        (A.product af ax)
    let (<*>) = apply
  end
  module _ : functor (A : BASIC) -> AUGMENTED = Augmented

end
module type APPLICATIVE = Applicative.AUGMENTED

module Monad = struct
  module type MONAD = sig
    type 'a t
    val pure : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
end
