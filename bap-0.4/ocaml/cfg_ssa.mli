(** 
    Static Single Assignment translation

    @todo Export trans_cfg, after finding a good name for it.

    @author Ivan Jager
*)


val of_astcfg : Cfg.AST.G.t -> Cfg.SSA.G.t

val of_ast : Ast.program -> Cfg.SSA.G.t

val to_astcfg : ?remove_temps:bool -> ?dsa:bool -> Cfg.SSA.G.t -> Cfg.AST.G.t

val to_ast : ?remove_temps:bool -> Cfg.SSA.G.t -> Ast.program


type translation_results = {
  cfg : Cfg.SSA.G.t;
  to_astvar: Var.t -> Var.t; (* Maps SSA vars back to the variable they came from *)
  to_ssavar: Var.t -> Var.t; (* Maps AST vars to SSA at end of exit node. *)
}  

val trans_cfg : Cfg.AST.G.t -> translation_results

