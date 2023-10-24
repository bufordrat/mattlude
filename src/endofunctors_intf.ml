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

module Functor = struct
  module type BASIC = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  end
end
module type FUNCTOR = Functor.AUGMENTED
               
module Applicative = struct
  module type BASIC = sig
    type 'a t
    include Functor.BASIC with type 'a t := 'a t
    val product : 'a t -> 'b t -> ('a * 'b) t
  end

  module type AUGMENTED = sig
    type 'a t
    include Functor.AUGMENTED with type 'a t := 'a t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val apply : ('a -> 'b) t -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  end
end
module type APPLICATIVE = Applicative.AUGMENTED

module type TRAVERSABLE = sig
  type 'a t
  include FUNCTOR with type 'a t := 'a t
  include FOLDABLE with type 'a t := 'a t
end

module Monad = struct
  module type BASIC = sig
    type 'a t
    val pure : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end

  module type AUGMENTED = sig
    type 'a t
    include BASIC with type 'a t := 'a t
    include Applicative.AUGMENTED with type 'a t := 'a t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >=> ) : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
    val ( <=< ) : ('a -> 'b t) -> ('c -> 'a t) -> 'c -> 'b t
    val ( >> ) : 'a t -> 'b t -> 'b t
    val join : 'a t t -> 'a t
  end
end
module type MONAD = Monad.AUGMENTED

module State = struct
  module type BASIC = sig
    type ('state, 'value) t
    val put : 'state -> ('state, unit) t
    val get : ('state, 'state) t
    val eval : ('state, 'value) t -> 'state -> 'value
    val exec : ('state, 'value) t -> 'state -> 'state
    val run : ('state, 'value) t -> 'state -> 'value * 'state
  end
end
