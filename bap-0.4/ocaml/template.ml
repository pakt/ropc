(* T E S T I N G *)

open Ast
open Big_int
open Int64
open Type
  
module D = Debug.Make(struct let name = "Template" and default=`Debug end)
open D

module type Types =
sig
  type environ
  type state
  type result_exp
  type result_stmt
end  

module Abstract (Typ : Types) =
struct

  type process_exp = Typ.environ -> Typ.state -> Typ.result_exp
  type process_stmt = Typ.environ -> Typ.state -> Typ.result_stmt
  
  module type Exprs =
  sig
    val var     : Var.t -> process_exp
    val int     : (big_int * typ) -> process_exp
    val lab     : string -> process_exp
    val ite     : (Ast.exp * Ast.exp * Ast.exp) -> process_exp
    val extract : (big_int * big_int * Ast.exp) -> process_exp
    val concat  : (Ast.exp * Ast.exp) -> process_exp
    val binop   : (binop_type * Ast.exp * Ast.exp) -> process_exp
    val unop    : (unop_type * Ast.exp) -> process_exp
    val cast    : (cast_type * typ * Ast.exp) -> process_exp
    val lett    : (Var.t * Ast.exp * Ast.exp) -> process_exp
    val load    : (Ast.exp * Ast.exp * Ast.exp * typ) -> process_exp
    val store   : (Ast.exp * Ast.exp * Ast.exp * Ast.exp * typ) -> process_exp
    val unknown : 'a -> process_exp
  end
    
  module type Stmts =
  sig
    val move    : (Var.t * Ast.exp * Ast.attrs) -> process_stmt
    val halt    : (Ast.exp * Ast.attrs) -> process_stmt
    val jmp     : (Ast.exp * Ast.attrs) -> process_stmt
    val cjmp    : (Ast.exp * Ast.exp * Ast.exp * Ast.attrs) -> process_stmt
    val assertt : (Ast.exp * Ast.attrs) -> process_stmt
    val comment : 'a -> process_stmt
    val label   : 'a -> process_stmt
    val special : (string * Ast.attrs) -> process_stmt
  end
    
  module Make(Expr: Exprs)(Stmt: Stmts) = 
  struct  
    
    let expr = function
      | Var v     -> Expr.var v
      | Int i     -> Expr.int i
      | Lab l     -> Expr.lab l
      | Ite i     -> Expr.ite i
      | Extract e -> Expr.extract e
      | Concat c  -> Expr.concat c
      | BinOp b   -> Expr.binop b
      | UnOp u    -> Expr.unop u
      | Cast c    -> Expr.cast c
      | Let l     -> Expr.lett l
      | Load l    -> Expr.load l
      | Store s   -> Expr.store s
      | Unknown u -> Expr.unknown u
      
    let stmt = function
      | Move m    -> Stmt.move m
      | Halt h    -> Stmt.halt h
      | Jmp j     -> Stmt.jmp j
      | CJmp c    -> Stmt.cjmp c
      | Assert a  -> Stmt.assertt a
      | Comment c -> Stmt.comment c
      | Label l   -> Stmt.label l
      | Special s -> Stmt.special s
  end  
end
