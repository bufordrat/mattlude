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
  module type FUNCTOR = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module type GOODIES = sig
    type 'a t
    include FUNCTOR with type 'a t := 'a t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val (<&>) : 'a t -> ('a -> 'b) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  end

  module Goodies (F : FUNCTOR) = struct
    include F 
    let ( let+ ) x f = map f x
    let (>>|) = ( let+ )
    let (<&>) = ( let+ )
    let (>|=) = ( let+ )
    let (<$>) = map
  end

  module Compose (F1 : FUNCTOR) (F2 : FUNCTOR) : FUNCTOR = struct
    type 'a t = 'a F2.t F1.t
    let map f composed = F1.map (F2.map f) composed
  end
end
module type FUNCTOR = Functor.FUNCTOR

module type TRAVERSABLE = sig
  type 'a t
  include FUNCTOR with type 'a t := 'a t
  include FOLDABLE with type 'a t := 'a t
end

module Applicative = struct
  module type APPLICATIVE = sig
    type 'a t
    include FUNCTOR with type 'a t := 'a t
    val product : 'a t -> 'b t -> ('a * 'b) t
  end

  module type GOODIES = sig
    type 'a t
    include Functor.GOODIES with type 'a t := 'a t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  end

  module Goodies (A : APPLICATIVE) = struct
    include A
    include Functor.Goodies (A)
    type 'a t = 'a A.t
    let ( and+ ) = product
    let apply af ax = map
                        (fun (f, x) -> f x)
                        (product af ax)
    let (<*>) = apply
  end
end
module type APPLICATIVE = Applicative.APPLICATIVE

(* module Monad = struct
 *   module type MONAD = sig
 *     type 'a t
 *     val pure : 'a -> 'a t
 *     val bind : 'a t -> ('a -> 'b t) -> 'b t
 *   end
 * 
 * 
 * end *)
