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
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <&> ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  end
end
               
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

module Result = struct

  module type PRELUDE = sig
    type 'a t
    val ok : 'a -> 'a t
    val good : 'a t -> bool
    val bad : 'a t -> bool
    val get_ok : 'a t -> 'a
    val default : 'a -> 'a t -> 'a
    val reduce : 'a t list -> 'a list
    val always : 'a -> (unit -> 'b) -> 'b
    val to_bool : 'a t -> bool
    val to_option : 'a t -> 'a option

  (* not included because they are polymorphic in the error type:
   * error
   * get_error
   * on_error
   * ( >>/ )
   * ( or )
   * trap
   * trapc
   * witherr
   * witherrc
   * of_bool
   * some_error
   * Seq module *)
  
  (* not included because this library generates them with Monad.Make
   * bind
   * ( >>= )
   * map
   * ( >>| )
   * ( >=> )
   * join *)
  end

  module type STDLIB = sig
    type 'a t
  end

  module type AUGMENTED = sig
    type 'a t
    include PRELUDE with type 'a t := 'a t
    include STDLIB with type 'a t := 'a t
    include Monad.AUGMENTED with type 'a t := 'a t
  end
end
module type RESULT = Result.AUGMENTED
