module type PRELUDE = sig
  type 'a t

  val return : 'a -> 'a t
  val default : 'a -> 'a t -> 'a
  val something : 'a t -> bool
  val nothing : 'a t -> bool
  val on_none : 'a t -> (unit -> 'a) -> 'a t
  val ( >>/ ) : 'a t -> (unit -> 'a) -> 'a t
  val reduce : 'a t list -> 'a list
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val either : ('a -> 'b) -> 'b -> 'a t -> 'b
  val maybe : ('a -> unit) -> 'a t -> unit
  val call : 'a -> ('b -> 'a) t -> 'b -> 'a
  val catch : ?this:exn -> ('a -> 'b) -> 'a -> 'b t
  val to_bool : 'a t -> bool
  val to_exn : exn -> 'a t -> 'a
  val of_result : ('a, 'b) result -> 'a t
  val to_string : ('a -> string) -> 'a t -> string
  val print : ('a -> string) -> 'a t -> unit
  val random : (unit -> 'a) -> unit -> 'a t
  val none : 'a t
  val some : 'a -> 'a t
  val value : 'a t -> default:'a -> 'a
  val get : 'a t -> 'a
  val fold : none:'a -> some:('b -> 'a) -> 'b t -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
  val is_none : 'a t -> bool
  val is_some : 'a t -> bool
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val to_result : none:'e -> 'a t -> ('a, 'e) result
  val to_list : 'a t -> 'a list
  val to_seq : 'a t -> 'a Seq.t
end

module type STDLIB = sig
  type 'a t
  val fold : none:'a -> some:('b -> 'a) -> 'b t -> 'a
end

module type AUGMENTED = sig
  open Endofunctors_intf

  type 'a t
  include STDLIB with type 'a t := 'a t
  include PRELUDE with type 'a t := 'a t
  include Monad.AUGMENTED with type 'a t := 'a t

  val cat_options : 'a t list -> 'a list
end
