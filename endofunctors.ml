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

  module Augmented (F : BASIC) = struct
    include F 
    let ( let+ ) x f = map f x
    let ( >>| ) = ( let+ )
    let ( <&> ) = ( let+ )
    let ( >|= ) = ( let+ )
    let ( <$> ) = map
  end
  module _ : functor (F : BASIC) -> AUGMENTED = Augmented

  module Compose (F1 : BASIC) (F2 : BASIC)
         : BASIC with type 'a t = 'a F2.t F1.t
    = struct
    type 'a t = 'a F2.t F1.t
    let map f composed = F1.map (F2.map f) composed
  end
end
module type FUNCTOR = Functor.BASIC

module type TRAVERSABLE = sig
  type 'a t
  include FUNCTOR with type 'a t := 'a t
  include FOLDABLE with type 'a t := 'a t
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

  module Augmented (A : BASIC)
         : AUGMENTED with type 'a t = 'a A.t = struct
    include Functor.Augmented (A)
    type 'a t = 'a A.t
    let ( and+ ) = A.product
    let apply af ax = map
                        (fun (f, x) -> f x)
                        (A.product af ax)
    let ( <*> ) = apply
  end

end
module type APPLICATIVE = Applicative.BASIC

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

  module Augmented (M : BASIC)
         : AUGMENTED with type 'a t = 'a M.t
    = struct
    let pure = M.pure
    let bind = M.bind
    let ( >>= ) = bind
    let ( let* ) = bind
    let ( >=> ) mf mg x = mf x >>= mg
    let ( <=< ) mf mg x = mg x >>= mf
    let ( >> ) mx my = mx >>= fun _ -> my

    module I = struct
      type 'a t = 'a M.t
      let map f mx = let* x = mx in
                     pure (f x)
      let product ax ay = let* x = ax in
                          let* y = ay in
                          pure (x,y)
    end
    include Applicative.Augmented (I)
  end
end
module type MONAD = Monad.BASIC

module Option = struct
  include Prelude.Option
  include Stdlib.Option

  let cat_options lst =
    let rec cat_options' acc = function
      | [] -> acc
      | Some x :: xs -> cat_options' (x :: acc) xs
      | None :: xs -> cat_options' acc xs
    in
    List.rev @@ cat_options' [] lst

  module OptionMonad = struct
    type 'a t = 'a option
    let pure = some
    let bind = bind
  end

  include OptionMonad
  include Monad.Augmented (OptionMonad)
end
