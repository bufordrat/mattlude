module type PURESTATE = State_intf.PURESTATE

module Make :
functor (S : PURESTATE) ->
  sig
    type ('s, 'a) t
    val pure : 'a -> (S.t, 'a) t
    val bind : (S.t, 'a) t -> ('a -> (S.t, 'b) t) -> (S.t, 'b) t
    val map : ('a -> 'b) -> (S.t, 'a) t -> (S.t, 'b) t
    val ( let+ ) : (S.t, 'a) t -> ('a -> 'b) -> (S.t, 'b) t
    val ( >>| ) : (S.t, 'a) t -> ('a -> 'b) -> (S.t, 'b) t
    val ( <&> ) : (S.t, 'a) t -> ('a -> 'b) -> (S.t, 'b) t
    val ( >|= ) : (S.t, 'a) t -> ('a -> 'b) -> (S.t, 'b) t
    val ( <$> ) : ('a -> 'b) -> (S.t, 'a) t -> (S.t, 'b) t
    val ( and+ ) : (S.t, 'a) t -> (S.t, 'b) t -> (S.t, 'a * 'b) t
    val apply : (S.t, 'a -> 'b) t -> (S.t, 'a) t -> (S.t, 'b) t
    val ( <*> ) : (S.t, 'a -> 'b) t -> (S.t, 'a) t -> (S.t, 'b) t
    val ( >>= ) : (S.t, 'a) t -> ('a -> (S.t, 'b) t) -> (S.t, 'b) t
    val ( let* ) : (S.t, 'a) t -> ('a -> (S.t, 'b) t) -> (S.t, 'b) t
    val ( >=> ) :
      ('a -> (S.t, 'b) t) -> ('b -> (S.t, 'c) t) -> 'a -> (S.t, 'c) t
    val ( <=< ) :
      ('a -> (S.t, 'b) t) -> ('c -> (S.t, 'a) t) -> 'c -> (S.t, 'b) t
    val ( >> ) : (S.t, 'a) t -> (S.t, 'b) t -> (S.t, 'b) t
    val join : (S.t, (S.t, 'a) t) t -> (S.t, 'a) t
    val put : 'state -> ('state, unit) t
    val get : ('state, 'state) t
    val eval : ('state, 'a) t -> 'state -> 'a
    val exec : ('state, 'a) t -> 'state -> 'state
    val run : ('state, 'a) t -> 'state -> 'a * 'state
  end
