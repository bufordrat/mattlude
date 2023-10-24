module type PRELUDE = sig

  (* Prelude values that are polymorphic in the error type are
     defined in MAKE below. *)

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
end

module type STDLIB = sig

  (* Stdlib values that are polymorphic in the error type are
     defined in MAKE below. *)

  type 'a t
  val value : 'a t -> default:'a -> 'a
  val to_option : 'a t -> 'a option
  val to_list : 'a t -> 'a list
  val to_seq : 'a t -> 'a Seq.t
end

module type ETUDE = sig
  open Endofunctors_intf
  
  type 'a t
  include Monad.AUGMENTED with type 'a t := 'a t
end

module type AUGMENTED = sig
  type 'a t
  include PRELUDE with type 'a t := 'a t
  include STDLIB with type 'a t := 'a t
  include ETUDE with type 'a t := 'a t
end

module type ERROR = sig type t end

module type MAKE = 
  functor (E : ERROR) -> sig
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

    include ETUDE with type 'a t := ('a, E.t) result
  end
