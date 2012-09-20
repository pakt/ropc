(** A module to perform coalescing on CFGs *)

open BatListFull
open Cfg

module D = Debug.Make(struct let name="Coalesce" and default=`Debug end)
open D

module MakeCoalesce (C: CFG) = 
struct
  module G = C.G
  module GS = Set.Make (struct 
                         type t = G.V.t 
                         let compare = Pervasives.compare
                        end)

 (* The function that does the coalescing. The algorithm is 
  * pretty simple:      
  *  - While doing a DFS we join two nodes n1 and n2 if
  *    a) n1 has only one successor and
  *    b) n2 has n1 as its only predecessor 
  *)  
  let coalesce cfg =
   let entry_node = C.find_vertex cfg BB_Entry in
   let visited = ref GS.empty in
   let isspecial v = match G.V.label v with BB _ -> false | _ -> true in
   let add_visited v = visited := GS.add v !visited in
   let rec fold_dfs g v =
     (* FIXME: this should be linear time, rather than quadratic *)
     if GS.mem v !visited then g
     else
      let worklist, g = 
       match G.succ g v with
       | [suc] as l -> 
         (* FIXME: consider substituting G.pred *)
         (match G.pred g suc with 
           | [_] when not(isspecial suc) ->
             (* Joining the nodes *)
             let curstmts = C.get_stmts g v in
             let sucstmts = C.get_stmts g suc in
             let mrgstmts = C.join_stmts curstmts sucstmts in
             let g = C.set_stmts g v mrgstmts in
             let sucsuc = G.succ_e g suc in
             (* Adding the edges to the new successors *)
             let g = List.fold_left (fun g e -> C.add_edge_e g (C.G.E.create v (C.G.E.label e) (C.G.E.dst e))) g sucsuc in
             (* Removing the successor *)
             let g = C.remove_vertex g suc in
             ([v], g)
           | _ -> add_visited v ; (l,g)
         )
       | l -> add_visited v ; (l,g)
     in
     List.fold_left fold_dfs g worklist
   in
   dprintf "Starting";
   let o = fold_dfs cfg entry_node in
   dprintf "Finished";
   o
end

module AST_Coalesce = MakeCoalesce(AST)
module SSA_Coalesce = MakeCoalesce(SSA)

