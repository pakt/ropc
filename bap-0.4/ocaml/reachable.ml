(** Code for dealing with reachable/unreachable nodes in a CFG. *)

module D = Debug.Make(struct let name = "Reachable" and default=`NoDebug end)

(** Along the lines of Builder.S, but with remove instead of add *)
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

(** Make functions for folding/iterating over reachable or unreachable vertices,
    and for removing them. *)
module Make (BI:B) =
struct

  type gt = BI.G.t
  type vt = BI.G.V.t
    
 let iter_reachable f g v =
   let module D = Graph.Traverse.Dfs(BI.G) in
   D.prefix_component f g v

 let fold_reachable f g v a =
   let r = ref a in
   iter_reachable (fun v -> r := f v !r) g v;
   !r

 let fold_unreachable f g v a =
   let h = Hashtbl.create 57 in
   iter_reachable (fun v -> Hashtbl.add h v ()) g v;
   BI.G.fold_vertex (fun v a -> if Hashtbl.mem h v then a else f v a) g a

 let iter_unreachable f g v =
   fold_unreachable (fun v () -> f v) g v ()

 let reachable g v =
   fold_reachable (fun v vs -> v::vs) g v []

 let unreachable g v =
   fold_unreachable (fun v vs -> v::vs) g v []

 let remove_unreachable g v =
   let u = unreachable g v in
   let count = ref 0 in
   let g = List.fold_left (fun a v -> incr count; BI.remove_vertex a v) g u in
   D.dprintf "removed %d" !count;
   g

end



module SSA = Make(Cfg.SSA)
module AST = Make(Cfg.AST)
