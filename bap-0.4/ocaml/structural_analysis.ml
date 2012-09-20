(** Structural Analysis

    This is an implementation of structural analysis based on Advanced
    Compiler Design & Implementation by Steven S Muchnick.

    TODO: Add support for proper and improper intervals.
*)

module D = Debug.Make(struct let name = "Structural" and default=`NoDebug end)
open D


type region_type =
  | Block | IfThen | IfThenElse | Case | Proper
  | SelfLoop | WhileLoop | NaturalLoop | Improper

type node = BBlock of Cfg.bbid | Region of region_type * node list

module C = Cfg.AST

module Node =
struct
  type t = node
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end


let rtype2s = function
  | Block       -> "Block"
  | IfThen	-> "IfThen"
  | IfThenElse	-> "IfThenElse"
  | Case	-> "Case"
  | Proper	-> "Proper"
  | SelfLoop	-> "SelfLoop"
  | WhileLoop	-> "WhileLoop"
  | NaturalLoop	-> "NaturalLoop"
  | Improper    -> "Improper"

let rec node2s = function
  | BBlock b -> Cfg.bbid_to_string b
  | Region(rt, b::_) -> node2s b ^ "'"
  | Region _ -> failwith "eep, empty region?"

let nodes2s nodes = String.concat ", " (List.map node2s nodes)

module G = Graph.Imperative.Digraph.ConcreteBidirectional(Node)
module PC = Graph.Path.Check(G)
module DFS = Graph.Traverse.Dfs(G)

let printg g =
  if debug then (
    G.iter_vertex (fun v -> pdebug (node2s v)) g;
    G.iter_edges (fun a b -> dprintf "%s -> %s" (node2s a) (node2s b)) g;
  )


let graph_of_cfg c =
  let g = G.create ~size:(C.G.nb_vertex c) () in
  let w v = BBlock (C.G.V.label v) in
  C.G.iter_vertex (fun v -> G.add_vertex g (w v)) c;
  C.G.iter_edges (fun s d -> G.add_edge g (w s) (w d)) c;
  g


let find_backedges g entry =
  (* Quick and dirty *)
  let stack = Hashtbl.create 57
  and found = Hashtbl.create 57 in
  let rec dfs p v =
    if Hashtbl.mem stack v then
      Hashtbl.add found (p,v) ()
    else (
      Hashtbl.add stack v ();
      G.iter_succ (dfs v) g v;
      Hashtbl.remove stack v;
    )
  in
  dfs entry entry;
  found

let structural_analysis c =
  let g = graph_of_cfg c
  and entry = ref (BBlock Cfg.BB_Entry)
  and exit = ref (BBlock Cfg.BB_Exit) in
  let size = G.nb_vertex g in
  let structures = ref (G.fold_vertex (fun v l -> v::l) g [])
  and structof = Hashtbl.create size
  and post = Array.make size !entry
  and post_ctr = ref 0 in

  let compact g n nset =
    if debug then dprintf "compacting %s from %s"  (node2s n) (nodes2s nset);
    let nleft = ref (List.length nset)
    and last = ref 0 in
    Array.iter 
      (fun v ->
	 if !nleft > 0 && List.mem v nset then (
	   if !nleft = 1 then (post.(!last) <- n; post_ctr := !last; incr last);
	   decr nleft )
	 else
	   (post.(!last) <- v; incr last)
      )
      post;
  in
  let replace g node nodeset =
    let update_edge s d =
      (* this adds some extra edges to nodes that will soon be removed *)
      (* The check is to avoid creating self loops. *)
      if s != node then	G.add_edge g s node;
      if d != node then G.add_edge g node d
    in
    (* *)
    G.add_vertex g node;
    List.iter
      (fun v->
	 let succs = G.succ g v and preds = G.pred g v in
	 List.iter (update_edge v) succs;
	 List.iter (fun p -> update_edge p v) preds;
      ) nodeset;
    compact g node nodeset;
    let rm rmn =
      (* dprintf "Removing %s" (node2s rmn); *)
      (* If we are going to remove the entry node, set the entry node
	 to the new coalesced node. *)
      if rmn = !entry then
      	entry := node;
      G.remove_vertex g rmn
    in
    List.iter rm nodeset;
  (*let ctnode = create_node() in*)
    (* FIXME: ctedges *)
  in
  let reduce g rtype nodeset =
    let node = Region(rtype, nodeset) in
    if debug then dprintf "Making %s region %s out of %s" (rtype2s rtype) (node2s node) (nodes2s nodeset);
    replace g node nodeset;
    structures := node :: !structures;
    List.iter (fun n->Hashtbl.add structof n node) nodeset;
    node
  in

  let acyclic_region_type g node =
    (* Check for a Block containing node *)
    let rec succchain n nset =
      match G.succ g n with
      | [s] -> (match G.pred g s with
		| [_] -> succchain s (s::nset)
		| _ -> List.rev nset)
      | _ -> List.rev nset
    in
    let rec predchain n nset =
      match G.pred g n with
      | [p] -> (match G.succ g p with
		  [_] -> predchain p (p::nset)
		| _ -> nset)
      | _ -> nset
    in
    (* dprintf "Succ %s" (node2s node); *)
    match G.succ g node with
    | [_] | [] ->
	let nset = predchain node (succchain node [node]) in
	(match nset with
	 | _::_::_ -> Some(Block, nset)
	 | _ -> None )
    | [m;n] -> (* Check for IfThenElse *)
	(match G.succ g m, G.succ g n with
	 | [s], [s'] when s = s' ->
	     (match G.pred g m, G.pred g n with
	      | [_], [_] -> Some(IfThenElse, [node;m;n])
	      | _ -> failwith "structural_analysis: unimplemeted: Proper?" ) (* if the successors of an if-then-else have other parents *)
	 | [s],_ when s = n && G.pred g m = [node] ->
	     Some(IfThen, [node; m])
	 | _,[s] when s = m && G.pred g n = [node] ->
	     Some(IfThen, [node; n])
	 | _ ->
	     None
	       (* dprintf "acyclic_region: was checking %s" (node2s node);
	     printg g;
	     failwith "unimplemeted Proper." *)
	)
    | _ ->	
	failwith("structural_analysis: indirect jumps unsupported yet: "^node2s node)
  in
  let minimize_improper g node nset =
    printg g;
    failwith "structural_analysis: minimize_improper unimplemented"
  in
  let cyclic_region_type g node nset =
    dprintf "cyclic_region: checking %s. nset: %s" (node2s node) (nodes2s nset);
    match nset with
    | [_] ->
        dprintf "Successors: %s" (nodes2s (G.succ g node));
	if List.mem node (G.succ g node) then
	  Some(SelfLoop, nset)
	else (dprintf "cyclic_region: Node %s not in a cycle" (node2s node); None)
    | x::y::_ ->
	let pc = PC.create g in
	if List.exists (fun m -> not(PC.check_path pc node m) ) nset then
	  Some(Improper, minimize_improper g node nset)
	else
	  let m = if x <> node then x else y in
	  if List.length (G.succ g node) = 2 && List.length(G.succ g m) = 1
	    && List.length (G.pred g node) = 2 && List.length(G.pred g m) = 1
	  then Some(WhileLoop, nset)
	  else Some(NaturalLoop, nset)
    | [] -> failwith "structural_analysis: cyclic_region_type: nset cannot be empty"
  in
  let get_reachunder g n =
(* HERP DERP
    (* This is terribly unoptimized *)
    let module PC = Graph.Path.Check(G) in
    let module Op = Graph.Oper.I(G) in
    (*let gm = Op.mirror (G.copy g) in*)
    (* let backedges = find_backedges g !entry in *)
    let path = PC.check_path (PC.create g) in
    (*let is_backedge a b =
      (* FIXME: There's a better way to check for backedgeness, isn't there?. *)
      (* Hashtbl.mem backedges (a,b) *)
      path b a
    in*)
    let _ = dprintf "get_reachunder: %s original backpreds: %s" (node2s n) (nodes2s (G.succ g n)) in
    let backpreds = List.filter (fun p -> path p n) (G.succ g n) in
    dprintf "preds: %s" (nodes2s (G.pred g n));
    Hashtbl.iter (fun (a,b) _ -> dprintf "node: %s -> %s" (node2s a) (node2s b)) backedges;
    dprintf "get_reachunder: %s backpreds: %s" (node2s n) (nodes2s backpreds);
    let g' = G.copy g in
    let path = PC.check_path (PC.create g') in
    G.remove_vertex g' n;
    n :: G.fold_vertex (fun m l -> if List.exists (fun k -> path m k) backpreds then m::l else l) g' []
*)
    let path = PC.check_path (PC.create g) in
    let seesMe = n :: G.fold_vertex (fun m l -> if (path m n) then m::l else l) g [] in
    let iSee   = G.fold_vertex (fun m l -> if (path n m) then m::l else l) g [] in
    let sect   = List.filter (fun x -> List.mem x seesMe) iSee in
    dprintf "get_reachunder HERP DERP: %s\n" (nodes2s sect);
    sect
  in

  let rec doit () =
    let progress = ref false in
    DFS.postfix_component (fun v-> post.(!post_ctr) <- v; incr post_ctr) g !entry;
    post_ctr := 0;
    while G.nb_vertex g > 1 && !post_ctr < G.nb_vertex g
    do
      let n = post.(!post_ctr) in
      match acyclic_region_type g n with
      | Some(rtype, nodeset) ->
	  let p = reduce g rtype nodeset in
	  if List.mem !entry nodeset then
	    entry := p;
	  if List.mem !exit nodeset then
	    exit := p;
	  progress := true;
      | None ->
          dprintf "acyclic_region_type returned None";
	  let reachunder = get_reachunder g n in
	  match cyclic_region_type g n reachunder with
	  | Some(rtype, reachunder) ->
	      let p = reduce g rtype reachunder in
	      if List.mem !entry reachunder then
		entry := p;
	      if List.mem !exit reachunder then
		exit := p;
	      progress := true;
	  | None ->
	      incr post_ctr
    done;
    if G.nb_vertex g > 1 then
      (dprintf "Ended iteration with %d nodes" (G.nb_vertex g);
       if !progress then doit()
       else (
	 printg g;
	 wprintf "Stopped making progress. Implement Proper/Improper intervals to fix this.";
	 let p = reduce g Proper (G.fold_vertex (fun x y-> x::y) g []) in
	 entry := p;
       )
      )
  in
  doit();
  !entry
      
