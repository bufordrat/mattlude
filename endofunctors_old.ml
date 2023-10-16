open Prelude


module type SEMIGROUP = sig
  type 'a t
  val append : 'a t -> 'a t -> 'a t
end

module type MONOID = sig
  include SEMIGROUP
  val empty : 'a t
end

module type FOLDABLE = sig
  include MONOID
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val null : 'a t -> bool
end

module Functor = struct
  module type FUNCTOR = sig
    type 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end
  
  module Goodies (F : FUNCTOR) = struct
    let ( let+ ) x f = F.map f x
    let (>>|) = ( let+ )
    let (<&>) = ( let+ )
    let (>|=) = ( let+ )
    let (<$>) = F.map
  end

  module Compose (F1 : FUNCTOR) (F2 : FUNCTOR) = struct
    type 'a t = 'a F2.t F1.t
    let map f composed = F1.map (F2.map f) composed
  end
end
module type FUNCTOR = Functor.FUNCTOR

module type TRAVERSABLE = sig
  include FUNCTOR
  include FOLDABLE with type 'a t := 'a t
end

module Applicative = struct
  module type APPLICATIVE = sig
    include FUNCTOR
    val product : 'a t -> 'b t -> ('a * 'b) t
  end

  module Goodies (A : APPLICATIVE) = struct
    include Functor.Goodies (A)
    let ( and+ ) = A.product
    let apply af ax =
      A.map
        (fun (f, x) -> f x)
        (A.product af ax)
    let (<*>) = apply
  end
end
module type APPLICATIVE = Applicative.APPLICATIVE

module Monad = struct
  module type MONAD = sig
    type 'a t
    val pure : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
  
  module Goodies (M : MONAD) = struct
    let pure = M.pure
    let bind = M.bind
    let (>>=) = bind
    let (let*) = bind
    let (>=>) mf mg x = mf x >>= mg
    let (<=<) mf mg x = mg x >>= mf
    let (>>) mx my = mx >>= fun _ -> my
    end

    module ToApplicative (M : MONAD) = struct
      include Goodies (M)

      (* reduction of applicative interface to monadic interface *)
      let map f mx = let* x = mx in
                     pure (f x)
      let product ax ay = let* x = ax in
                          let* y = ay in
                          pure (x,y)

      module I = struct
        type 'a t = 'a M.t
        let map = map
        let product = product
      end

      include Applicative.Goodies (I)
      open Fun
      let ( <* ) ax ay = pure const <*> ax <*> ay
      let ( *> ) ax ay = pure (flip const) <*> ax <*> ay
    end
end
module type MONAD = Monad.MONAD

module List = struct
  include Prelude.List
  include Stdlib.List

  module ListMonad = struct
    type 'a t = 'a list
    let pure x = x :: []
    let bind mx k = flatten (map k mx)
  end

  module Traverse = struct
    module Make (M : MONAD) = struct
      open Monad.ToApplicative (M)
      let sequence lst =
        let reducer acc mx =
          let+ x = mx
          and+ xs = acc
          in x :: xs
        in
        foldl reducer (M.pure []) lst >>| rev
      let traverse f xs = sequence (List.map f xs)
      let forM xs f = traverse f xs
    end
  end
                  
  include ListMonad
  include Monad.ToApplicative (ListMonad)
  include Traverse.Make (ListMonad)
end
module _ : MONAD = List
         
(* helper functions for optional values *)
module Option = struct
  include Prelude.Option
  include Stdlib.Option

  (* unwraps the Somes; throws the None-s out *)
  let cat_options lst =
    let rec cat_options' acc = function
      | [] -> acc
      | Some x :: xs -> cat_options' (x :: acc) xs
      | None :: xs -> cat_options' acc xs
    in
    List.rev @@ cat_options' [] lst

  (* for auto-generating monad and applicative stuff *)
  module OptionMonad = struct
    type 'a t = 'a option
    let pure = some
    let bind = bind
  end

  include OptionMonad
  include Monad.ToApplicative (OptionMonad)
  include List.Traverse.Make (OptionMonad)
end
module _ : MONAD = Option

module Result = struct
  (* module functor for building a Result module with cool extra stuff
     in it; takes a module containing the error type as an input *)
  module type ERROR = sig
    type t
  end

  module Make (E : ERROR) = struct
    include Stdlib.Result
    include Prelude.Result
    
    (* for auto-generating monad and applicative stuff *)
    module ResultMonad = struct
      type 'a t = ('a, E.t) result
      let pure = Result.ok
      let bind = Result.bind
    end

    (* TODO: figure out why including ResultMonad here breaks my broken parser *)
    include Monad.ToApplicative (ResultMonad)
    include List.Traverse.Make (ResultMonad)

    module _ : MONAD = ResultMonad
  end
end

module State = struct
  module type PURESTATE = sig
    type t
  end
  
  module Make (S : PURESTATE) = struct
    module StateMonad = struct
      type 'a t = S.t -> ('a * S.t)
      let pure x state = (x, state)
      let bind mx k state1 =
        let ( result1, state2 ) = mx state1 in
        k result1 @@ state2
    end

    include StateMonad
    include Monad.ToApplicative (StateMonad)
    include List.Traverse.Make (StateMonad)

    let put value _ = ((), value)
    let get state = (state, state)

    let eval mx state = mx state |> fst
    let exec mx state = mx state |> snd

    module _ : MONAD = StateMonad
  end
end
