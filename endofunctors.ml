module type SEMIGROUP =
  Endofunctors_intf.SEMIGROUP

module type MONOID =
  Endofunctors_intf.MONOID

module type FOLDABLE =
  Endofunctors_intf.FOLDABLE

module Functor = struct
  module type BASIC =
    Endofunctors_intf.Functor.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Functor.AUGMENTED

  module Make (F : BASIC)
         : AUGMENTED with type 'a t = 'a F.t
    = struct
    include F 
    let ( let+ ) x f = map f x
    let ( >>| ) = ( let+ )
    let ( <&> ) = ( let+ )
    let ( >|= ) = ( let+ )
    let ( <$> ) = map
  end

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
  module type BASIC =
    Endofunctors_intf.Applicative.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Applicative.AUGMENTED

  module Make (A : BASIC)
         : AUGMENTED with type 'a t = 'a A.t = struct
    include Functor.Make (A)
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
  module type BASIC =
    Endofunctors_intf.Monad.BASIC

  module type AUGMENTED =
    Endofunctors_intf.Monad.AUGMENTED

  module Make (M : BASIC)
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
    include Applicative.Make (I)
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
  include Monad.Make (OptionMonad)
end

module Result = struct
  module type ERROR = sig type t end

  module type AUGMENTED =
    Endofunctors_intf.RESULT

  module Make (E : ERROR) : sig
    type 'a t = ('a, E.t) result
    include AUGMENTED with type 'a t := 'a t
    end = struct
    include Prelude.Result
    include Stdlib.Result

    module ResultMonad = struct
      type 'a t = ('a, E.t) result
      let pure = Result.ok
      let bind = Result.bind
    end
    include Monad.Make (ResultMonad)
  end
end

(* module State = struct
 *   module type PURESTATE = sig
 * 
 *   end
 * 
 *   module Make = struct
 *   module StateMonad = struct
 *     type 'a t = S.t -> ('a * S.t)
 *     let pure x state = (x, state)
 *     let bind mx k state1 =
 *       let ( result1, state2 ) = mx state1 in
 *       k result1 @@ state2
 *   end
 * 
 *   include Monad.Make (StateMonad)
 * 
 * 
 * end *)
