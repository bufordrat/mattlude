module type ERROR = sig type t end

module type AUGMENTED =
  Result_intf.AUGMENTED

module Make (E : ERROR) : sig
  type 'a t = ('a, E.t) result

  (* Prelude values that are polymorphic in the error type go
       here. *)
  val error : E.t -> ('a, E.t) result
  val get_error : ('a, E.t) result -> E.t
  val on_error :
    ('a, E.t) result ->
    (E.t -> ('a, 'c) result) ->
    ('a, 'c) result
  val ( >>/ ) :
    ('a, E.t) result ->
    (E.t -> ('a, 'c) result) ->
    ('a, 'c) result
  val ( or ) :
    ('a, E.t) result ->
    ('a, 'c) result ->
    ('a, 'c) result
  val trap : (exn -> E.t) -> ('b -> 'c) -> 'b -> ('c, E.t) result
  val trapc : E.t -> ('b -> 'c) -> 'b -> ('c, E.t) result
  val witherr : ('a -> E.t) -> ('c, 'a) result -> ('c, E.t) result
  val witherrc : E.t -> ('b, 'c) result -> ('b, E.t) result
  val of_bool : ?err:E.t -> E.t -> bool -> (E.t, E.t) result
  val of_option : E.t -> 'b option -> ('b, E.t) result
  val some_error : ('a, E.t) result -> E.t option
  include AUGMENTED with type 'a t := ('a, E.t) result

  (* Stdlib values that are polymorphic in the error type go
       here *)
  val map_error : ('e -> E.t) ->
                  ('a, 'e) result ->
                  ('a, E.t) result
  val fold : ok:('a -> 'c) ->
             error:(E.t -> 'c) ->
             ('a, E.t) result ->
             'c
  val equal :
    ok:('a -> 'a -> bool) ->
    error:(E.t -> E.t -> bool) ->
    ('a, E.t) result ->
    ('a, E.t) result ->
    bool
  val compare :
    ok:('a -> 'a -> int) ->
    error:(E.t -> E.t -> int) ->
    ('a, E.t) result ->
    ('a, E.t) result ->
    int
end = struct
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
