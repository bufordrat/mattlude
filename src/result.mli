module type ERROR = sig type t end

module Make :
  functor (E : ERROR) ->
    sig
      type 'a t = ('a, E.t) result
      val error : E.t -> ('a, E.t) result
      val get_error : ('a, E.t) result -> E.t
      val on_error :
        ('a, E.t) result -> (E.t -> ('a, 'c) result) -> ('a, 'c) result
      val ( >>/ ) :
        ('a, E.t) result -> (E.t -> ('a, 'c) result) -> ('a, 'c) result
      val ( or ) : ('a, E.t) result -> ('a, 'c) result -> ('a, 'c) result
      val trap : (exn -> E.t) -> ('b -> 'c) -> 'b -> ('c, E.t) result
      val trapc : E.t -> ('b -> 'c) -> 'b -> ('c, E.t) result
      val witherr : ('a -> E.t) -> ('c, 'a) result -> ('c, E.t) result
      val witherrc : E.t -> ('b, 'c) result -> ('b, E.t) result
      val of_bool : ?err:E.t -> E.t -> bool -> (E.t, E.t) result
      val of_option : E.t -> 'b option -> ('b, E.t) result
      val some_error : ('a, E.t) result -> E.t option
      val ok : 'a -> ('a, E.t) result
      val good : ('a, E.t) result -> bool
      val bad : ('a, E.t) result -> bool
      val get_ok : ('a, E.t) result -> 'a
      val default : 'a -> ('a, E.t) result -> 'a
      val reduce : ('a, E.t) result list -> 'a list
      val always : 'a -> (unit -> 'b) -> 'b
      val to_bool : ('a, E.t) result -> bool
      val value : ('a, E.t) result -> default:'a -> 'a
      val to_option : ('a, E.t) result -> 'a option
      val to_list : ('a, E.t) result -> 'a list
      val to_seq : ('a, E.t) result -> 'a Seq.t
      val pure : 'a -> ('a, E.t) result
      val bind :
        ('a, E.t) result -> ('a -> ('b, E.t) result) -> ('b, E.t) result
      val map : ('a -> 'b) -> ('a, E.t) result -> ('b, E.t) result
      val ( let+ ) : ('a, E.t) result -> ('a -> 'b) -> ('b, E.t) result
      val ( >>| ) : ('a, E.t) result -> ('a -> 'b) -> ('b, E.t) result
      val ( <&> ) : ('a, E.t) result -> ('a -> 'b) -> ('b, E.t) result
      val ( >|= ) : ('a, E.t) result -> ('a -> 'b) -> ('b, E.t) result
      val ( <$> ) : ('a -> 'b) -> ('a, E.t) result -> ('b, E.t) result
      val ( and+ ) :
        ('a, E.t) result -> ('b, E.t) result -> ('a * 'b, E.t) result
      val apply :
        ('a -> 'b, E.t) result -> ('a, E.t) result -> ('b, E.t) result
      val ( <*> ) :
        ('a -> 'b, E.t) result -> ('a, E.t) result -> ('b, E.t) result
      val ( >>= ) :
        ('a, E.t) result -> ('a -> ('b, E.t) result) -> ('b, E.t) result
      val ( let* ) :
        ('a, E.t) result -> ('a -> ('b, E.t) result) -> ('b, E.t) result
      val ( >=> ) :
        ('a -> ('b, E.t) result) ->
        ('b -> ('c, E.t) result) -> 'a -> ('c, E.t) result
      val ( <=< ) :
        ('a -> ('b, E.t) result) ->
        ('c -> ('a, E.t) result) -> 'c -> ('b, E.t) result
      val ( >> ) : ('a, E.t) result -> ('b, E.t) result -> ('b, E.t) result
      val join : (('a, E.t) result, E.t) result -> ('a, E.t) result
      val map_error : ('e -> E.t) -> ('a, 'e) result -> ('a, E.t) result
      val fold : ok:('a -> 'c) -> error:(E.t -> 'c) -> ('a, E.t) result -> 'c
      val equal :
        ok:('a -> 'a -> bool) ->
        error:(E.t -> E.t -> bool) ->
        ('a, E.t) result -> ('a, E.t) result -> bool
      val compare :
        ok:('a -> 'a -> int) ->
        error:(E.t -> E.t -> int) ->
        ('a, E.t) result -> ('a, E.t) result -> int
    end
