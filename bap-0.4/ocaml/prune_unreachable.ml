(** Code for removing unreachable nodes in a CFG. *)

module D = Debug.Make(struct let name = "Prune_unreachable" and default=`Debug end)
module R = Reachable

let prune_unreachable_ast g =
  R.AST.remove_unreachable g (Cfg.AST.G.V.create Cfg.BB_Entry)

let prune_unreachable_ssa g =
  R.SSA.remove_unreachable g (Cfg.SSA.G.V.create Cfg.BB_Entry)

