(** Control flow graphs
    @author Ivan Jager
*)

(** A basic block identifier. *)
type bbid =
    BB_Entry (** entry node *)
  | BB_Exit   (** return/exit node *)
  | BB_Indirect (** indirect jump to/from a node *)
  | BB_Error (** jump target when an error occurs *)
  | BB of int (** for normal basic blocks *)
(** If a node has BB_Indirect as it's successor it means it is an indirect
    jump. If it has BB_Indirect as it's predecessor, it means it is the
    possible target of an indirect jump. *)


val bbid_to_string : bbid -> string

module BBid :
  sig
    type t = bbid
    val compare : 'a -> 'a -> int
    val hash : bbid -> int
    val equal : 'a -> 'a -> bool
  end


module type CFG =
sig
  include Graph.Builder.S with type G.V.label = bbid and type G.E.label = bool option

  type lang

  (** Finds a vertex by a bbid *)
  val find_vertex : G.t -> G.V.label -> G.V.t

  (** Finds a vertex by a label in its stmts *)
  val find_label : G.t -> Type.label -> G.V.t

  (** Gets the statements from a basic block *)
  val get_stmts : G.t -> G.V.t -> lang

  (** Sets the statements for a basic block *)
  val set_stmts : G.t -> G.V.t -> lang -> G.t

  (** Joins two statement lists *)
  val join_stmts : lang -> lang -> lang

  (** Generate a new ID that wasn't previously generated for the given graph *)
  (* val newid : G.t -> bbid *)

  (** Creates a new vertex with new ID and adds it to the graph
      with the given statements. *)
  val create_vertex : G.t -> lang -> G.t * G.V.t

  (* extra builder-like stuff *)
  val remove_vertex : G.t -> G.V.t -> G.t
  val remove_edge : G.t -> G.V.t -> G.V.t -> G.t
  val remove_edge_e : G.t -> G.E.t -> G.t

end


module AST : CFG with type lang = Ast.stmt list
module SSA : CFG with type lang = Ssa.stmt list


(**)
(* These are for cfg_ast.ml and cfg_ssa.ml to be able to translate without
   breaking nextid. Poke Ivan if you think you need them for something else. *)
val map_ast2ssa : (Ast.stmt list -> Ssa.stmt list) -> AST.G.t -> SSA.G.t
val map_ssa2ast : (Ssa.stmt list -> Ast.stmt list) -> SSA.G.t -> AST.G.t
