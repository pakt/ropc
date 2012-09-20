(** A module to perform chopping on SSAs *)

open Cfg
open Depgraphs

module CHOP_SSA =
struct

 (* Test-printing of ASTs TODO:remove *)
 (* TODO: Create a new file only for chopping *)

 module Traverse = Graph.Traverse.Dfs(Cfg.SSA.G)

  let print_cfg cfg = 
   let print_stmts v =
    let stmts = SSA.get_stmts cfg v in
    List.iter
     (fun s -> 
       Printf.printf "chopped: %s\n" (Pp.ssa_stmt_to_string s)
     ) stmts
   in
   Traverse.prefix print_stmts cfg
    
(* Simple chopping implementation *)

module SG =
struct
 type t = Cfg.SSA.G.t
 module V = Cfg.SSA.G.V
 let iter_vertex = Cfg.SSA.G.iter_vertex
 let iter_succ = Cfg.SSA.G.iter_succ
end

module Comp = Graph.Components.Make(SG);;

(* Calculating the strongly connected component *)
 let get_scc cfg src trg = 
 (* Adding a temporary back-edge *)
  let (edgeadded,cfg) = 
   if SSA.G.mem_edge cfg trg src then (false,cfg)
   else (true,SSA.add_edge cfg trg src) in
   (* Keeping the strongly connected component *)
   let sccs = Comp.scc_list cfg in
   let scclist = List.find (List.mem src) sccs in
   if not (List.mem trg scclist) then failwith "sink node unreachable" ;
   let scc = 
    SSA.G.fold_vertex 
     (fun v g -> 
       if not (List.mem v scclist) then SSA.remove_vertex g v
       else g
     ) cfg cfg
   in
   (* Removing the back-edge *)
   if edgeadded then SSA.remove_edge scc trg src else scc

 let compute_cds cfg h =
  let cdg = CDG_SSA.compute_cdg cfg in
   Cfg.SSA.G.iter_edges
    (fun v1 v2 ->
      let ss1 = SSA.get_stmts cfg v1 in
      let ss2 = SSA.get_stmts cfg v2 in
      let num = (List.length ss1) - 1 in
      ignore 
       (List.fold_left
        (fun id _ ->
          Hashtbl.add h (v2,id) (v1,num) ;
          id + 1
        ) 0 ss2
       )
    ) cdg

(* Adding jump stmts that are not directly control-dependent
 * on conditional jumps to get a more precise slice.
 * We use the conservative algorithm presented in Figure 13 of
 * 'On Slicing Programs with Jump Statements'
 * --> http://portal.acm.org/citation.cfm?id=178456            *)
 let add_jmp_stmts cfg dds =
  Cfg.SSA.G.iter_edges
   (fun v1 v2 ->
     try
      let stmts1 = SSA.get_stmts cfg v1 in
      match List.hd (List.rev stmts1) with
      | Ssa.CJmp _ ->
        let num = List.length stmts1 - 1 in
        let stmts2 = SSA.get_stmts cfg v2 in
        ignore
        (List.fold_left
         (fun id s ->
           match s with 
           | Ssa.Jmp _ -> Hashtbl.add dds (v2,id) (v1,num) ; id + 1
           | _ -> id + 1
         ) 0 stmts2
        )
      | _ -> ()
     with Failure "hd" -> ()
   ) cfg

 let get_dds cfg =
  let dds = Hashtbl.create 5700 in
  DDG_SSA.compute_dds cfg dds ;
  compute_cds cfg dds ;
  add_jmp_stmts cfg dds ;
  dds
  
  (* Slicing the cfg *)
  let slice cfg node stmt =
   let deps = get_dds cfg in
   let rec get_reachable visited wl =
    match wl with
    | [] -> 
      visited
    | x::worklist ->
      if DDG_SSA.SS.mem x visited then get_reachable visited worklist
      else 
       let next = Hashtbl.find_all deps x in
       get_reachable (DDG_SSA.SS.add x visited) (next@worklist)
   in
   let vis = get_reachable DDG_SSA.SS.empty [(node,stmt)] in
   let not_changable = function
    | Ssa.Label _ | Ssa.Comment _ -> true
    | _ -> false
   in
   let tmp = SSA.G.fold_vertex
    (fun v g ->
      let _,newstmts = 
       List.fold_left
        (fun (id,acc) s -> 
         if not_changable s || DDG_SSA.SS.mem (v,id) vis then (id+1,s::acc)
         else (id+1,acc)
        ) (0,[]) (SSA.get_stmts cfg v)
      in
      SSA.set_stmts g v (List.rev newstmts)
    ) cfg cfg
   in
   print_cfg tmp ;
   tmp

  (* Performing chopping from a source to a sink *)
 let chop cfg srcbb _srcn trgbb trgn = 
  let get_v num = 
   try SSA.find_vertex cfg (BB num)
   with Not_found -> 
    failwith "input does not correspond to basic blocks"
  in
  let src = get_v srcbb 
  and trg = get_v trgbb in
  let scc = get_scc cfg src trg in
 (* Adding entry and exit nodes *)
  let entry = SSA.find_vertex cfg BB_Entry 
  and exit = SSA.find_vertex cfg BB_Exit in
  let scc = SSA.add_vertex scc entry in
  let scc = SSA.add_vertex scc exit in
  let scc = SSA.add_edge scc entry src in
  let scc = SSA.add_edge scc trg exit in
  slice scc trg trgn
end

