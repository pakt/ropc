open Ast
type t =
  | Assume of exp
  | Assign of Var.t * exp
  | Assert of exp
  | Choice of t * t
  | Seq of t * t
  | Skip

(*val of_straightline : ?acc:t -> Ast.stmt list -> t*)
val of_astcfg : ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> t
val of_ast : Ast.program -> t
val remove_skips : t -> t

val passified_of_ssa :
  ?entry:Cfg.SSA.G.V.t -> ?exit:Cfg.SSA.G.V.t -> Cfg.SSA.G.t -> t * var list
val passified_of_astcfg :
  ?entry:Cfg.AST.G.V.t -> ?exit:Cfg.AST.G.V.t -> Cfg.AST.G.t -> t * var list * (Var.t->Var.t)
