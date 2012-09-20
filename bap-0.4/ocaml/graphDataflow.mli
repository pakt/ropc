(** Dataflow module for use with the ocamlgraph library

    @author Ivan Jager
*)


(** the types of graphs we work on *)
module type G =
  sig
    type t
    module V : Graph.Sig.COMPARABLE
    val pred : t -> V.t -> V.t list
    val succ : t -> V.t -> V.t list
    val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  end

(** dataflow directions *)
type direction = Forward | Backward


(** Dataflow doesn't need a full latice, but we do need this much.
    http://en.wikipedia.org/wiki/Meet-semilattice
 *)
module type BOUNDED_MEET_SEMILATTICE =
sig
  
  (** The type of a latice element *)
  type t
    
  (** Top of the latice *)
  val top : t
    
  (** The meet operator.
      [meet v1 v2] should form a lattice. in particular,
      remember that meet v1 Top = meet Top v1 = v1, 
      and meet Bottom _ = meet _ Bottom = Bottom
  *)
  val meet : t -> t -> t

  (** Equality checking for the latice.
      Returns true when the two latice elements are the same.
  *)
  val equal : t -> t -> bool
end

(** A dataflow problem is defined by a lattice over a graph. *)
module type DATAFLOW =
  sig

    module L : BOUNDED_MEET_SEMILATTICE
    module G : G

    (** The transfer function over node elements, e.g., statements *)
    val transfer_function : G.t -> G.V.t -> L.t -> L.t
      
    (** the starting node for the analysis *)
    val s0 : G.V.t

    (** the initial value for analysis. This is what s0 should start
	out with. All other nodes start out with Top *)
    val init : G.t -> L.t

    (** the dataflow direction *)
    val dir : direction
  end

(** Make(DATAFLOW) returns a worklist algorithm, which when applied to
    a graph, returns a pair of functions. The first function f1 is given
    v,  returns in[v].  The function f2 is for v, return out[v] *)
module Make :
  functor (D : DATAFLOW) ->
    sig
      val worklist_iterate : ?init:(D.G.t -> D.L.t) ->
        D.G.t -> (D.G.V.t -> D.L.t) * (D.G.V.t -> D.L.t)
    end
