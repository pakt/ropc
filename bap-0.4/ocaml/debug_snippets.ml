(**
   Snippets of debugging code
*)

module D = Debug.Make(struct let name = "Debug_snippets" and default=`Debug end)
open D

(** AST debugging visitor *)

let v = object(self)
  inherit Ast_visitor.nop
    (* Add each variable to definedvars *)
  method visit_stmt stmt =
    dprintf "Stmt: %s" (Pp.ast_stmt_to_string stmt);
    `SkipChildren
end 
  
let print_astcfg p =
  ignore(Ast_visitor.cfg_accept v p)

let print_ast p =
  ignore(Ast_visitor.prog_accept v p)

let intv_to_string (i,t) =
  let e = Ast.Int(i,t) in
  Printf.sprintf "%s" (Pp.ast_exp_to_string e)
