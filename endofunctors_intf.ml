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
  end

end

module Result = struct

  module type PRELUDE = sig
    type 'a t
    val good : 'a t -> bool
    val bad : 'a t -> bool
    val get_ok : 'a t -> 'a
    val default : 'a -> 'a t -> 'a
    val reduce : 'a t list -> 'a list

  (* not included because they are polymorphic in the error type:
   * get_error
   * 
   * 
   * not included because this library generates them with Monad.Make
   * bind
   * ( >>= )
   * map
   * ( >>| ) *)
  end

  module type AUGMENTED = sig
    type 'a t
    include PRELUDE with type 'a t := 'a t
    include Monad.AUGMENTED with type 'a t := 'a t
  end
end
module type RESULT = Result.AUGMENTED
