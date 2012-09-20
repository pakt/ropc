(** Visitor for SSA

    The design of this visitor was highly influenced by the one from CIL.
 *)

open Type
open Ssa

(** The type for a visitor *)
class type t =
object
  method visit_exp : exp -> exp visit_action
    (** Called when visiting an expression *)
    
  method visit_stmt : stmt -> stmt visit_action
    (** Called when visiting a statement.
	FIXME: would be nice to be able to add stmts... We may change this. *)
    
  method visit_value : value -> value visit_action
    (** Called when visiting a value *)

  method visit_rvar : var -> var visit_action
    (** Called when visiting a refenenced variable. (IE: inside an expression) *)

  method visit_avar : var -> var visit_action
    (** Called when visiting an assigned variable. (IE: On the LHS of a Move *)
end

(** A nop visitor similar to [nop_bap_visitor].
    See {!Vine.nop_bap_visitor} for more information.   *)
class nop : t


val exp_accept : #t -> exp -> exp
val rvar_accept : #t -> var -> var
val avar_accept : #t -> var -> var
val value_accept : #t -> value -> value
val stmt_accept : #t -> stmt -> stmt
val stmts_accept : #t -> stmt list -> stmt list
val prog_accept : #t -> Cfg.SSA.G.t -> Cfg.SSA.G.t
