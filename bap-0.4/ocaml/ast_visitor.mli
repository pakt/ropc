(** Visitor for the AST *)

open Type
open Ast

(** The type for a visitor *)
class type t =
object
  method visit_exp : exp -> exp visit_action
    (** Called when visiting an expression *)
    
  method visit_stmt : stmt -> stmt visit_action
    (** Called when visiting a statement.
	FIXME: would be nice to be able to add stmts... We may change this. *)
    
  method visit_rvar : var -> var visit_action
    (** Called when visiting a refenenced variable. (IE: inside an expression) *)

  method visit_avar : var -> var visit_action
    (** Called when visiting an assigned variable. (IE: On the LHS of a Move *)
    
  method visit_binding : var * exp -> (var * exp) visit_action
    (** Called when mapping a variable in a let *)

  method visit_uvar : var -> var visit_action
    (** Called when unmapping a variable in a let *)

end

(** A nop visitor similar to [nop_bap_visitor].
    See {!Vine.nop_bap_visitor} for more information.   *)
class nop : t


val stmt_accept : #t -> stmt -> stmt
val exp_accept : #t -> exp -> exp
val rvar_accept : #t -> var -> var
val avar_accept : #t -> var -> var
val binding_accept : #t -> var * exp -> var * exp
val uvar_accept : #t -> var -> var
val prog_accept : #t -> program -> program
val cfg_accept : #t -> Cfg.AST.G.t -> Cfg.AST.G.t
