(** 
    The type used for variables and functions to create and use them.
 *)

module D = Debug.Make(struct let name="Var" and default=`Debug end)
open D

module V = struct

  type t = V of int * string * Type.typ
    (** The type for a variable identifier.
	The int should uniquely identify the variable. The string is simply to make
	it easier for humans to read.
	The type is included here so that it doesn't need to be in the lval.
	
	Please try to recycle the same var rather than creating multiple copies
	of it, as that is a waste of memory. In other words, if two vars refer to
	the same thing, they should be [==].
    *)


  let hash (V(i,_,_)) = i

  let equal = ( == )

  (* is it faster to use the generic compare, or < and = ? *)
  let compare (V(x,_,_)) (V(y,_,_)) = compare x y
end

include V

module VarHash = Hashtbl.Make(V)
module VarMap = Map.Make(V)
module VarSet = Set.Make(V)



(** Create a new "unused" variable with the given name as a base. *)
let newvar =
  let varcounter = ref 0 in
  (fun s t ->
     let n = !varcounter in
     if n = -1 then failwith "newvar: counter wrapped around";
     (varcounter := n+1;
      V(n,s,t))
  )


(** Create a new unused variable with the same name and type as the given one *)
let renewvar (V(_,name,t)) = newvar name t

(** Get the type of a variable *)
let typ (V(_,_,t)) = t

(** Get the name of a variable *)
let name (V(_,n,_)) = n
