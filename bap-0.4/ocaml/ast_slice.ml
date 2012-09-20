(** A module to perform chopping on ASTs *)

open Cfg
open Depgraphs

module CHOP_AST =
struct

 (* Test-printing of ASTs TODO:remove *)
 (* TODO: Create a new file only for chopping *)

 module Traverse = Graph.Traverse.Dfs(Cfg.AST.G)

  let print_cfg cfg = 
   let print_stmts v =
    let stmts = AST.get_stmts cfg v in
    List.iter
     (fun s -> 
       Printf.printf "%s\n" (Pp.ast_stmt_to_string s)
     ) stmts
   in
   Traverse.prefix print_stmts cfg
    
(* Simple chopping implementation *)

module SG =
struct
 type t = Cfg.AST.G.t
 module V = Cfg.AST.G.V
 let iter_vertex = Cfg.AST.G.iter_vertex
 let iter_succ = Cfg.AST.G.iter_succ
end

module Comp = Graph.Components.Make(SG);;

(* Calculating the strongly connected component *)
 let get_scc cfg src trg = 
 (* Adding a temporary back-edge *)
  let (edgeadded,cfg) = 
   if AST.G.mem_edge cfg trg src then (false,cfg)
   else (true,AST.add_edge cfg trg src) in
   (* Keeping the strongly connected component *)
   let sccs = Comp.scc_list cfg in
   let scclist = List.find (List.mem src) sccs in
   if not (List.mem trg scclist) then failwith "sink node unreachable" ;
   let scc = 
    AST.G.fold_vertex 
     (fun v g -> 
       if not (List.mem v scclist) then AST.remove_vertex g v
       else g
     ) cfg cfg
   in
   (* Removing the back-edge *)
   if edgeadded then AST.remove_edge scc trg src else scc

 let compute_cds cfg h =
  let cdg = CDG_AST.compute_cdg cfg in
   Cfg.AST.G.iter_edges
    (fun v1 v2 ->
      let ss1 = AST.get_stmts cfg v1 in
      let ss2 = AST.get_stmts cfg v2 in
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
  Cfg.AST.G.iter_edges
   (fun v1 v2 ->
     try
      let stmts1 = AST.get_stmts cfg v1 in
      match List.hd (List.rev stmts1) with
      | Ast.CJmp _ ->
        let num = List.length stmts1 - 1 in
        let stmts2 = AST.get_stmts cfg v2 in
        ignore
        (List.fold_left
         (fun id s ->
           match s with 
           | Ast.Jmp _ -> Hashtbl.add dds (v2,id) (v1,num) ; id + 1
           | _ -> id + 1
         ) 0 stmts2
        )
      | _ -> ()
     with Failure "hd" -> ()
   ) cfg

 let get_dds cfg =
  let dds = Hashtbl.create 5700 in
  DDG_AST.compute_dds cfg dds ;
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
      if DDG_AST.SS.mem x visited then get_reachable visited worklist
      else 
       let next = Hashtbl.find_all deps x in
       get_reachable (DDG_AST.SS.add x visited) (next@worklist)
   in
   let vis = get_reachable DDG_AST.SS.empty [(node,stmt)] in
   let not_changable = function
    | Ast.Label _ | Ast.Comment _ -> true
    | _ -> false
   in
   let tmp = AST.G.fold_vertex
    (fun v g ->
      let _,newstmts = 
       List.fold_left
        (fun (id,acc) s -> 
         if not_changable s || DDG_AST.SS.mem (v,id) vis then (id+1,s::acc)
         else (id+1,acc)
        ) (0,[]) (AST.get_stmts cfg v)
      in
      AST.set_stmts g v (List.rev newstmts)
    ) cfg cfg
   in
   print_cfg tmp ;
   tmp

  (* Performing chopping from a source to a sink *)
 let chop cfg srcbb _srcn trgbb trgn = 
  let get_v num = 
   try AST.find_vertex cfg (BB num)
   with Not_found -> 
    failwith "input does not correspond to basic blocks"
  in
  let src = get_v srcbb 
  and trg = get_v trgbb in
  let scc = get_scc cfg src trg in
 (* Adding entry and exit nodes *)
  let entry = AST.find_vertex cfg BB_Entry 
  and exit = AST.find_vertex cfg BB_Exit in
  let scc = AST.add_vertex scc entry in
  let scc = AST.add_vertex scc exit in
  let scc = AST.add_edge scc entry src in
  let scc = AST.add_edge scc trg exit in
  slice scc trg trgn
end

