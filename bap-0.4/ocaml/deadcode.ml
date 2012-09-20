(** Dead code elimination for SSA graphs. *)

(* Based off Vine_dataflow.DeadCode *)
open Ssa

module Debug = Debug.Make(struct let name = "DeadCode" and default=`NoDebug end)
module VH = Var.VarHash
module C = Cfg.SSA
module BH = Hashtbl.Make(C.G.V)

type site = C.G.V.t * Ssa.stmt
	

    
(* return list of lvals defined and used by this stmt *)
(* XXX doesn't match behavior of previous implementation
   w.r.t. Set. Not sure which behavior is correct. *)
(* not performing union on used vars; shouldn't be needed
   for correctness, since double-counting will be symmetrical
   when incrementing and decrementing use-counts *)
let def_uses s =
  let lv, liveout = 
    match s with
    | Move (lv, _, a) when List.mem Type.Liveout a -> ([lv],[lv])
    | Move (lv, _, _) -> ([lv],[])
    | _ -> ([],[])
  in
  let uses = ref [] in
  let vis =  object(self)
    inherit Ssa_visitor.nop
    method visit_rvar v = uses := v :: !uses;
      `DoChildren
  end
  in
  ignore (Ssa_visitor.stmt_accept vis s);
  (lv, !uses, liveout)


(* in SSA, a variable is live at its definition site iff its list of
   uses is not empty. Therefore, calculating live variables is really
   just a matter of calculating whether or not a variable has any
   uses. (p445  ML Tiger book ) *)
(** Performs dead code elimination, returning the new CFG and a bool
    indicating whether anything changed. Any move with the [Liveout]
    attribute will be assumed to be live.

    @param globals a list of additional variables to be considered
    live out. The safe default is to declare all variables live-out. 
*)
let do_dce ?(globals=[]) graph =
  let (var_to_deps: Ssa.var list VH.t) = VH.create 57 in
  let (var_to_defsite: site VH.t) = VH.create 57 in
  let (usecounts: int ref VH.t) = VH.create 57 in
  let usecount v =
    try VH.find usecounts v
    with Not_found ->
      let r = ref 0 in
      VH.add usecounts v r;
      r
  in
  let deps = VH.find var_to_deps in
  (* get initial mappings *)
  C.G.iter_vertex
    (fun bb ->
       let stmts = C.get_stmts graph bb in
       List.iter
	 (fun s ->
	    let site = (bb, s) in
            let defs, deps, liveout = def_uses s in
            
            (* iterate over defs, updating maps *)
            List.iter
              (fun defd_var ->
                 assert(not (VH.mem var_to_deps defd_var));
                 assert(not (VH.mem var_to_defsite defd_var));
                 VH.add var_to_deps defd_var deps;
                 VH.add var_to_defsite defd_var site;
                 ignore(usecount defd_var); (* Add 0 if needed *)
              )
              defs;
            
            (* update usecounts mapping *)
            List.iter (fun v -> incr (usecount v)) deps;
	    (* increment liveout vars by one, so they are never dead *)
            List.iter (fun v -> incr (usecount v)) liveout;
	 )
         stmts
    )
    graph;

  (* increment use-counts for globals *)
  List.iter (fun g -> incr (usecount g))  globals;

  (* initialize kill list with unused defs *)
  let to_kill = Stack.create() in
  VH.iter (fun v c -> if !c = 0 then Stack.push v to_kill) usecounts;

  (* we'll end up eliminating dead code iff vars_to_kill is non-empty *)
  let has_changed = not(Stack.is_empty to_kill) in
  
  (* iteratively kill stmts defining dead vars, adding newly dead vars
     to var_to_kill, until vars_to_kill is empty *)
  let (dead_sites:(site, unit) Hashtbl.t) = Hashtbl.create 5700 in
  let blocks_to_update = BH.create 5 in
  while not(Stack.is_empty to_kill) do 
    let var_to_kill = Stack.pop to_kill in

    if VH.mem var_to_defsite var_to_kill then (
      (* add defining stmt to kill list *)
      let (bb,stmt) as site_to_kill = VH.find var_to_defsite var_to_kill in
      assert(not (Hashtbl.mem dead_sites site_to_kill));
      Hashtbl.add dead_sites site_to_kill ();
      BH.replace blocks_to_update bb ();

      (* decrement uses of used vars, 
         adding to kill list if uses is now 0 *)
      List.iter
        (fun used_var ->
           let use_count = usecount used_var in
           assert(!use_count > 0);
           decr use_count;
           if !use_count = 0 then Stack.push used_var to_kill;
        )
        (deps var_to_kill)
    ) else
      Debug.dprintf "Dead var %s is undefined" (Pp.var_to_string var_to_kill)
  done;
  
  (* go over graph to remove dead sites *)
  Debug.dprintf "Deleting %d dead stmts" (Hashtbl.length dead_sites);
  let graph =
    BH.fold
      (fun bb () graph->
	 let stmts = C.get_stmts graph bb in
	 let stmts' =
           List.filter (fun s -> not (Hashtbl.mem dead_sites (bb,s))) stmts
       in
	 C.set_stmts graph bb stmts'
    )
    blocks_to_update
    graph
  in
  (graph, has_changed)


