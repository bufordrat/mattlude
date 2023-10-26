module type PURESTATE = sig type t end

module type BASIC =
  State_intf.BASIC

module type MONAD =
  Endofunctors_intf.Monad.AUGMENTED

module Make (S : PURESTATE) : sig
  type ('s, 'a) t
  include MONAD with type 'a t := (S.t, 'a) t
  include BASIC with type ('s, 'a) t := ('s, 'a) t
end = struct

  type ('s, 'a) t = 's -> 'a * 's
  module StateMonad = struct
    type 'a t = S.t -> 'a * S.t
    let pure x state = (x, state)
    let bind mx k state1 =
      let ( result1, state2 ) = mx state1 in
      k result1 @@ state2
  end

  let put state _ = ((), state)
  let get state = (state, state)
  
  let eval mx state = mx state |> fst
  let exec mx state = mx state |> snd
  let run mx state = mx state

  open Endofunctors
  module S = Monad.Make (StateMonad)

  let pure = S.pure
  let bind = S.bind
  let map = S.map
  let ( let+ ) = S.( let+ )
  let ( >>| ) = S.( >>| )
  let ( <&> ) = S.( <&> )
  let ( >|= ) = S.( >|= )
  let ( <$> ) = S.( <$> )
  let ( and+ ) = S.( and+ )
  let product = S.product
  let ( <*> ) = S.( <*> )
  let apply = S.apply
  let ( >>= ) = S.( >>= )
  let ( let* ) = S.( let* )
  let ( >=> ) = S.( >=> )
  let ( <=< ) = S.( <=< )
  let ( >> ) = S.( >> )
  let join = S.join
end
