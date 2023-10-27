module type PRELUDE = sig
  type 'a t
  val len : 'a list -> int
  val comparison : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  val to_string :
    ?left:string ->
    ?sep:string -> ?right:string -> ('a -> string) -> 'a list -> string
  val head : 'a list -> 'a option
  val tail : 'a list -> 'a list option
  val last : 'a list -> 'a
  val get : int -> 'a list -> 'a
  val snoc : 'a list -> 'a -> 'a list
  val consup : 'a -> 'a list
  val revcons : 'a -> 'a list -> 'a list
  val unfoldr : ('a -> bool) -> ('a -> 'b) -> ('a -> 'a) -> 'a -> 'b list
  val make : int -> (int -> 'a) -> 'a list
  val repeat : int -> 'a -> 'a list
  val trappend : 'a list -> 'a list -> 'a list
  val prepend : 'a list -> 'a list -> 'a list
  val postpend : 'a list -> 'a list -> 'a list
  val scanl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a list
  val eachpair : ('a -> 'a -> 'b) -> 'a list -> 'b list
  val permutations : 'a list -> 'a list list
  val upto : int -> int -> int list
  val ( -- ) : int -> int -> int list
  val random : ?size:(unit -> int) -> (unit -> 'a) -> unit -> 'a list
  val null : 'a list -> bool
  val empty : 'a list -> bool
  val nonempty : 'a list -> bool
  val singleton : 'a list -> bool
  val many : 'a list -> bool
  val prefix : 'a list -> 'a list -> bool
  val foldl : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
  val foldl1 : ('a -> 'a -> 'a) -> 'a list -> 'a
  val foldr1 : ('a -> 'a -> 'a) -> 'a list -> 'a
  val foldl2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val foldr2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val foldwise :
    ?skip:bool -> int -> ('a -> 'b list -> 'a) -> 'a -> 'b list -> 'a
  val conswith : ('a -> 'b) -> 'a -> 'b list -> 'b list
  val conswhen : ('a -> bool) -> 'a -> 'a list -> 'a list
  val snocwith : ('a -> 'b) -> 'b list -> 'a -> 'b list
  val snocwhen : ('a -> bool) -> 'a list -> 'a -> 'a list
  val anded : bool list -> bool
  val ored : bool list -> bool
  val conjunction : ('a -> bool) list -> 'a -> bool
  val disjunction : ('a -> bool) list -> 'a -> bool
  val all : ('a -> bool) -> 'a list -> bool
  val any : ('a -> bool) -> 'a list -> bool
  val sum : int list -> int
  val maximumBy : ?compare:('a -> 'a -> int) -> 'a list -> 'a
  val maximum : 'a list -> 'a
  val minimum : ?compare:('a -> 'a -> int) -> 'a list -> 'a
  val break : ('a -> 'a -> bool) -> 'a list -> 'a list list
  val flatmap : ('a -> 'b list) -> 'a list -> 'b list
  val delete : ?eq:('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
  val replace : 'a -> 'a -> 'a list -> 'a list
  val behead : 'a list -> 'a list -> 'a list
  val prefixes : 'a list -> 'a list list
  val suffixes : 'a list -> 'a list list
  val intersperse : 'a -> 'a list -> 'a list
  val pad : ?left:bool -> def:'a -> int -> 'a list -> 'a list
  val transpose : ?def:'a -> 'a list list -> 'a list list
  val evens : 'a list -> 'a list
  val odds : 'a list -> 'a list
  val splitat : int -> 'a list -> 'a list * 'a list
  val everyother : 'a list -> 'a list
  val take : int -> 'a list -> 'a list
  val drop : int -> 'a list -> 'a list
  val takeall : int -> 'a list -> 'a list list
  val splitwhile : ('a -> bool) -> 'a list -> 'a list * 'a list
  val takewhile : ('a -> bool) -> 'a list -> 'a list
  val dropwhile : ('a -> bool) -> 'a list -> 'a list
  val one_of : 'a list -> 'a -> bool
  val zip : 'a list -> 'b list -> ('a * 'b) list
  val zipwith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val unzip : ('a * 'b) list -> 'a list * 'b list
  module Assoc = Prelude.Lists.Assoc
  val sorted : ('a -> 'a -> int) -> 'a list -> bool
  val uniq : ?compare:('a -> 'a -> int) -> 'a list -> 'a list
  val uniqc : ?compare:('a -> 'a -> int) -> 'a list -> (int * 'a) list
  val index : ?z:int -> 'a list -> (int * 'a) list
  val pos : ?eq:('a -> 'a -> bool) -> 'a -> 'a list -> int
  val project : ?relaxed:bool -> int list -> 'a list -> 'a list
  val nub : ?compare:('a -> 'a -> int) -> 'a list -> 'a list
  val nub2 : ?compare:('a -> 'a -> int) -> 'a list -> 'a list
  val union : ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val intersect :
    ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val subset : ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> bool
  val diff : ?compare:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  val cartesian_product : 'a list list -> 'a list list
  val powerset : 'a list -> 'a list list
  val combinations : int -> 'a list -> 'a list list
end

module type STDLIB = sig
  type 'a t
  val length : 'a t -> int
  val compare_lengths : 'a t -> 'b t -> int
  val compare_length_with : 'a t -> int -> int
  val cons : 'a -> 'a t -> 'a t
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val nth : 'a t -> int -> 'a
  val nth_opt : 'a t -> int -> 'a option
  val rev : 'a t -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val rev_append : 'a t -> 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val flatten : 'a t t -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val rev_map : ('a -> 'b) -> 'a t -> 'b t
  val filter_map : ('a -> 'b option) -> 'a t -> 'b t
  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
  val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b t -> 'a * 'c t
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val rev_map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a t -> bool
  val exists : ('a -> bool) -> 'a t -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val mem : 'a -> 'a t -> bool
  val memq : 'a -> 'a t -> bool
  val find : ('a -> bool) -> 'a t -> 'a
  val find_opt : ('a -> bool) -> 'a t -> 'a option
  val find_map : ('a -> 'b option) -> 'a t -> 'b option
  val filter : ('a -> bool) -> 'a t -> 'a t
  val find_all : ('a -> bool) -> 'a t -> 'a t
  val filteri : (int -> 'a -> bool) -> 'a t -> 'a t
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  val partition_map : ('a -> ('b, 'c) Either.t) -> 'a t -> 'b t * 'c t
  val assoc : 'a -> ('a * 'b) t -> 'b
  val assoc_opt : 'a -> ('a * 'b) t -> 'b option
  val assq : 'a -> ('a * 'b) t -> 'b
  val assq_opt : 'a -> ('a * 'b) t -> 'b option
  val mem_assoc : 'a -> ('a * 'b) t -> bool
  val mem_assq : 'a -> ('a * 'b) t -> bool
  val remove_assoc : 'a -> ('a * 'b) t -> ('a * 'b) t
  val remove_assq : 'a -> ('a * 'b) t -> ('a * 'b) t
  val split : ('a * 'b) t -> 'a t * 'b t
  val combine : 'a t -> 'b t -> ('a * 'b) t
  val sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val fast_sort : ('a -> 'a -> int) -> 'a t -> 'a t
  val sort_uniq : ('a -> 'a -> int) -> 'a t -> 'a t
  val merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  val to_seq : 'a t -> 'a Seq.t
  val of_seq : 'a Seq.t -> 'a t
end

module type ETUDE = sig
  type 'a t
  include Traverse_intf.Traversable.List.AUGMENTED
          with type 'a t := 'a t
end

module type AUGMENTED = sig
  type 'a t
  include PRELUDE with type 'a t := 'a t
  include STDLIB with type 'a t := 'a t
  include ETUDE with type 'a t := 'a t
end
