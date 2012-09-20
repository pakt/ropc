type t = private V of int * string * Type.typ

val newvar : string -> Type.typ -> t
val renewvar : t -> t

val typ : t -> Type.typ
val name : t -> string


val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int

module VarHash : Hashtbl.S with type key = t
module VarMap : Map.S with type key = t
module VarSet : Set.S with type elt = t
