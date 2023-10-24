module type ERROR = sig type t end

module type AUGMENTED =
  Result_intf.AUGMENTED

module type MAKE =
  Result_intf.MAKE

module Make : MAKE =
  functor (E : ERROR) ->
  struct
  open Endofunctors

  include Prelude.Result
  include Stdlib.Result

  module ResultMonad = struct
    type 'a t = ('a, E.t) result
    let pure = Stdlib.Result.ok
    let bind = Stdlib.Result.bind
  end
  include Monad.Make (ResultMonad)
end
