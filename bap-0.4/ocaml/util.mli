val id : 'a -> 'a
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)
val ( <@ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val foldn : ?t:int -> ('a -> int -> 'a) -> 'a -> int -> 'a
val foldn64 : ?t:int64 -> ('a -> int64 -> 'a) -> 'a -> int64 -> 'a
val mapn : (int -> 'a) -> int -> 'a list

(* Use decr and incr instead
val inc : int ref -> unit
val dec : int ref -> unit
*)

val list_union : 'a list -> 'a list -> 'a list
val list_intersection : 'a list -> 'a list -> 'a list
val list_does_intersect : 'a list -> 'a list -> bool
val shortest_first : (int -> int -> 'a) -> 'b list -> 'c list -> 'a
val list_difference : 'a list -> 'a list -> 'a list
val list_subset : 'a list -> 'a list -> bool
val list_set_eq : 'a list -> 'a list -> bool

val union_find : ('a -> 'b list) -> 'a list -> 'a list list

val list_foldl : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val list_pop : 'a list ref -> 'a
val list_push : 'a list ref -> 'a -> unit
val list_last : 'a list -> 'a
val list_partition_last : 'a list -> 'a list * 'a
val list_last_option : 'a list -> 'a option
val list_filter_some : ('a -> 'b option) -> 'a list -> 'b list
val list_find_some : ('a -> 'b option) -> 'a list -> 'b
val list_unique : 'a list -> 'a list
val list_map_some : ('a -> 'b option) -> 'a list -> 'b list
val list_join : ('a -> 'a -> 'a) -> 'a list -> 'a
val list_firstindex : ?s:int -> 'a list -> ('a -> bool) -> int
val list_insert : 'a list -> 'a list -> int -> 'a list
val list_remove: 'a list -> int -> int -> 'a list
val list_delete: 'a list -> 'a -> 'a list
val list_compare: ('a -> 'a -> int) -> ('a list) -> ('a list) -> int
val list_memf: ('a -> 'a -> bool) -> 'a -> ('a list) -> bool
val list_cart_prod2: ('a -> 'b -> unit) -> ('a list) -> ('b list) -> unit
val list_cart_prod3: ('a -> 'b -> 'c -> unit) -> ('a list) -> ('b list) -> ('c list) -> unit
val list_cart_prod4: ('a -> 'b -> 'c -> 'd -> unit) -> ('a list) -> ('b list) -> ('c list) -> ('d list) -> unit
val list_permutation: 'a list list -> ('a list -> unit) -> unit
val list_existssome: ('a -> 'b option) -> ('a list) -> 'b option
val list_for_allsome: ('a -> 'b option) -> ('a list) -> 'b list option

val get_hash_keys : ?sort_keys:bool -> ('a, 'b) Hashtbl.t -> 'a list
val change_ext : string -> string -> string
val list_directory : ?sort_files:bool -> string -> string list

val split_common_prefix : 'a list -> 'a list -> 'a list * 'a list * 'a list
val split_common_suffix : 'a list -> 'a list -> 'a list * 'a list * 'a list

val option_unwrap : ('a option) -> 'a
val option_map : ('a -> 'b) -> 'a option -> 'b option
val apply_option : ('a -> 'a) option -> 'a -> 'a
val has_some: 'a option -> bool

val print_separated_list : ('a -> string) -> string -> 'a list -> string

module HashUtil :
  functor (H : Hashtbl.S) ->
    sig
      val hashtbl_eq : ?eq:('a -> 'a -> bool) -> 'a H.t -> 'a H.t -> bool
    end
val hashtbl_eq :
  ?eq:('a -> 'a -> bool) -> ('b, 'a) Hashtbl.t -> ('b, 'a) Hashtbl.t -> bool

val trim_newline : string -> string


val int64_udiv : int64 -> int64 -> int64
val int64_urem : int64 -> int64 -> int64
val int64_ucompare : int64 -> int64 -> int
val int64_umax : int64 -> int64 -> int64
val int64_umin : int64 -> int64 -> int64

val run_with_remapped_fd :
  Unix.file_descr -> Unix.file_descr -> (unit -> 'a) -> 'a

val take : int -> 'a list -> 'a list
val fast_append : 'a list -> 'a list -> 'a list

module StatusPrinter :
  sig
    val init : string -> int -> unit
    val inc  : unit -> unit
    val stop : unit -> unit
  end

val binary_of_int64 : ?pad:int -> int64 -> string
val binary_of_big_int : ?pad:int -> Big_int.big_int -> string
val hex_of_big_int : ?pad:int -> Big_int.big_int -> string
val big_int_of_string : string -> Big_int.big_int
