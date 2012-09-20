(** Reachability analysis

$Id: reachable.mli 5241 2011-09-21 23:20:19Z edmcman $
*)
module type B =
sig
  module G : Graph.Traverse.G

  val remove_vertex : G.t -> G.V.t -> G.t

end

module type Reach =
  sig

    type gt
    type vt

    val iter_reachable : (vt -> unit) -> gt -> vt -> unit
    val iter_unreachable : (vt -> unit) -> gt -> vt -> unit
      
    val fold_reachable : (vt -> 'a -> 'a) -> gt -> vt -> 'a -> 'a
    val fold_unreachable : (vt -> 'a -> 'a) -> gt -> vt -> 'a -> 'a
      
    val reachable : gt -> vt -> vt list
    val unreachable : gt -> vt -> vt list
      
    val remove_unreachable : gt -> vt -> gt      
  end

module Make :
  functor (BI : B) ->
    Reach with type gt = BI.G.t and type vt = BI.G.V.t

module AST : (Reach with type gt = Cfg.AST.G.t and type vt = Cfg.AST.G.V.t)
module SSA : (Reach with type gt = Cfg.SSA.G.t and type vt = Cfg.SSA.G.V.t)

