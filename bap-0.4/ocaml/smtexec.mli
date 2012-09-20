(**
   Module for executing SMTs inside of BAP

   XXX: This is not portable in any shape or way. It will only work on
   Unix-like systems.

   @author ejs
*)

type result = Valid | Invalid | SmtError | Timeout
val result_to_string : result -> string

module type SOLVER_INFO =
sig
  val timeout : int (** Default timeout in seconds *)
  val solvername : string (** Solver name *)
  val cmdstr : string -> string (** Given a filename, produce a command string to invoke solver *)
  val parse_result : ?printmodel:bool -> string -> string -> Unix.process_status -> result (** Given output, decide the result *)
  val printer : Formulap.fppf
end

(** This is a hack so we can use subtyping of the solver instances *)
class type smtexec =
object
  method printer : Formulap.fppf
  method solvername : string
  method solve_formula_file : ?printmodel:bool -> ?timeout:int -> string -> result
end

module type SOLVER =
sig
  val solve_formula_file : ?printmodel:bool -> ?timeout:int -> string -> result (** Solve a formula in a file *)
  val check_exp_validity : ?timeout:int -> ?exists:(Ast.var list) -> ?foralls:(Ast.var list) -> Ast.exp -> result (** Check validity of an exp *)
    (* XXX: check_exp_sat *)
    (** Write a formula for weakest precondition.

        XXX: Select weakest precondition method
        XXX: Give this a better name
    *)
  val create_cfg_formula :
    ?exists:Ast.var list ->  ?foralls:Ast.var list -> ?remove:bool -> Cfg.AST.G.t -> string
    (* XXX: solve_wp *)
    
  val si : smtexec
end

module Make : functor (Module : SOLVER_INFO) -> SOLVER

module STP : SOLVER
module STPSMTLIB : SOLVER
module CVC3 : SOLVER
module CVC3SMTLIB : SOLVER
module YICES : SOLVER

(** A Hashtbl of solver names to solver interfaces *)
val solvers : (string,smtexec) Hashtbl.t


(* The following are deprecated, use modules *)

(* val runstp : ?timeout:int -> string -> result *)

(* possibly expose write_formula too if that is useful *)

(* val query_formula : *)
(*   ?timeout:int -> ?exists:Ast.var list ->  ?foralls:Ast.var list *)
(*   -> Ast.exp -> result *)
