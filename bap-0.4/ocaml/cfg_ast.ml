(** Translations between AST programs and AST CFGs.


    TODO: Coalescing; Use BB_Entry when making traces, but avoid joining that
    trace with the trace containing BB_Exit.
*)

open Type
open Ast
open Cfg
open BatListFull

module C = Cfg.AST


module D = Debug.Make(struct let name = "CFG_AST" and default=`NoDebug end)
open D

let v2s v = bbid_to_string(C.G.V.label v)

let create c l stmts =
  let v = C.G.V.create l in
  let c = C.add_vertex c v in
  (C.set_stmts c v stmts, v)

(** Find BB_Entry in a graph, or raise an exception if not already present. *)
let find_entry g =
  try
    g, C.find_vertex g BB_Entry
  with Not_found ->
    failwith "BB_Entry is missing! This should not happen."

(** Find BB_Error in a graph, or add it if not already present. *)
let find_error g =
  try
    g, C.find_vertex g BB_Error
  with Not_found ->
    create g BB_Error [Label(Name "BB_ERROR", []); Assert(exp_false, [])]

(** Find BB_Exit in a graph, or add it if not already present. *)
let find_exit g =
  try
    g, C.find_vertex g BB_Exit
  with Not_found ->
    create g BB_Exit [(*Label(Name "BB_Exit", []); *)Comment("exit node",[])]

(** Find BB_Indirect in a graph, or add it if not already present. *)
let find_indirect g =
  try
    g, C.find_vertex g BB_Indirect
  with Not_found ->
    create g BB_Indirect []

(** Build a CFG from a program *)
let of_prog ?(special_error = true) p =
  let (tmp, entry) = create (C.empty()) BB_Entry [Comment("entry node",[])] in
  let (tmp, exit) = find_exit tmp in
  let (tmp, error) = find_error tmp in
  let (c, indirect) = find_indirect tmp in
  let c = C.add_edge c indirect error in (* indirect jumps could fail *)

  let postponed_edges = Hashtbl.create 5700 in
  let indirect_target = ref false in
  let add_indirect = function
    | Addr _ -> indirect_target := true
    | Name _ -> ()
  in
  let add_new c revstmts addpred =
    let c,v = C.create_vertex c (List.rev revstmts) in
    dprintf "of_prog: added vertex %s" (v2s v);
    let c = 
      if !indirect_target then (
      indirect_target := false;
      C.add_edge c indirect v
    ) else c
    in
    match addpred with
    | None -> (c,v)
    | Some v' -> (C.add_edge c v' v, v)
  in
  (* Decide what to do given the current state and the next stmt.
     c: the cfg so far
     cur: reversed list of statements we will add to the next bb
     onlylabs: true if cur only contains labels (or comments)
     addpred: Some v when v should fall through to the next bb
  *)
  let f (c, cur,onlylabs, addpred) s =
    let g () =
      let (c,v) = add_new c (s::cur) addpred in
      let for_later ?lab t = Hashtbl.add postponed_edges v (lab,t) in
      let c = match s with
	| Jmp(t, _) -> for_later t; c
	| CJmp(_,t,f,_) -> for_later ~lab:true t; for_later ~lab:false f; c
	| Special _ -> C.add_edge c v error
	| Halt _ -> C.add_edge c v exit
	| _ -> failwith "impossible"
      in
      (c, [], true, None)
    in
    match s with
    | Jmp _ | CJmp _ | Halt _ ->
	g ()
    | Special _ when special_error ->
	g ()
    | Special _ (* specials are not error *) ->
	(c, s::cur, onlylabs, addpred)
    | Label(l,_) when onlylabs ->
	add_indirect l;
	(c, s::cur, true, addpred)
    | Label(l,_) ->
	add_indirect l;
	let c,v = add_new c cur addpred in
	(c, [s], true, Some v)
    | Move _ | Assert _ ->
	  (c, s::cur, false, addpred)
    | Comment _ ->
	(c, s::cur, onlylabs, addpred)
  in
  let (c,last,_,addpred) = List.fold_left f (c,[],true,Some entry) p in
  let c = match last with
    | _::_ ->
	let c,v = add_new c last addpred in
	C.add_edge c v exit
    | [] -> match addpred with
      | None -> c
      | Some v -> C.add_edge c v exit (* Should only happen for empty programs *)
  in
  let make_edge v (lab,t) c =
    let dst = lab_of_exp t in
    let tgt = match dst with
      | None -> indirect
      | Some l ->
	  try (C.find_label c l)
	  with Not_found ->
	    wprintf "Jumping to unknown label: %s" (Pp.label_to_string l);
	    error
      (* FIXME: should jumping to an unknown address be an error or indirect? *)
    in
    C.add_edge_e c (C.G.E.create v lab tgt)
  in
  let c = Hashtbl.fold make_edge postponed_edges c in
  (* FIXME: Colescing *)
  c

(** Convert a CFG back to an AST program.
    This is needed for printing in a way that can be parsed again.
*)
let to_prog c =
  let size = C.G.nb_vertex c in
  let module BH = Hashtbl.Make(C.G.V) in
  let tails = BH.create size (* maps head vertex to the tail of the trace *)
    (* maps vertex to succ it was joined with, forming a trace *)
  and joined = BH.create size
  and hrevstmts = BH.create size in
  let get_revstmts b =
    try BH.find hrevstmts b
    with Not_found ->
      let s = List.rev (C.get_stmts c b) in
      BH.add hrevstmts b s;
      s
  in
  C.G.iter_vertex (fun v -> BH.add tails v v) c;
  let bh_find_option h b = try Some(BH.find h b) with Not_found->None in
  let rec grow_trace cond head =
      match bh_find_option tails head with
      | None ->
	  () (* must have already been joined previously*)
      | Some tail ->
	  assert(not(BH.mem joined tail));
	  let rec find_succ = function
	    | [] -> ()
	    | suc::rest ->
		match bh_find_option tails suc with
		| Some succtail when cond tail suc ->
		    dprintf "to_prog: joining %s with %s" (v2s tail) (v2s suc);
		    BH.add joined tail suc;
		    BH.replace tails head (BH.find tails suc);
		    BH.remove tails suc;
		    grow_trace cond head
		| _ -> (* suc is part of another trace, or cond failed *)
		    find_succ rest
	  in
	  find_succ (C.G.succ c tail)
  in
  let grow_traces cond =
    let worklist = BH.fold (fun k _ w -> k::w) tails [] in
    List.iter (grow_trace cond) worklist
  in
  let normal v =
    match C.G.V.label v with | BB _ -> true | _ -> false
  in
  let joinable v =
    match C.G.V.label v with | BB _ -> true | BB_Exit -> true | _ -> false
  in
  let has_jump src =
    match get_revstmts src with
      | (Jmp _ | CJmp _)::_ -> true
      | _ -> false
  in
  let labs = BH.create size
  and newlabs = BH.create size in
  let get_label b =
    try BH.find labs b
    with Not_found ->
      let rec find_label = function
	| Label(l,[])::_ -> Some l
	| Comment _ :: xs -> find_label xs
	| _ -> None
      in
      match find_label (C.get_stmts c b) with
      | Some l ->
	  BH.add labs b l;
	  l
      | None ->
	  let l = newlab () in
	  BH.add newlabs b l;
	  BH.add labs b l;
	  l
  in
  let ensure_jump src dst =
    if not(has_jump src)
    then match C.G.succ c src with
	| [d] ->
	    assert (C.G.V.equal dst d);
	    let j = Jmp(exp_of_lab (get_label dst), []) in
	    BH.replace hrevstmts src (j::get_revstmts src)
	| _ ->
	    failwith("Cfg_ast.to_prog: no jump at end of block with > 1 succ: "
		     ^ v2s src)
  in
  (* join traces without jumps *)
  grow_traces (fun b suc -> normal b && joinable suc && not(has_jump b));
  (* join other traces (if we cared, we could remove some jumps later) *)
  grow_traces (fun b suc -> normal b && joinable suc);
  (* join the entry node, NOT with the trace containing the exit *)
  grow_trace
    (fun b suc ->
       let suctail = BH.find tails suc in
       C.G.V.label suctail <> BB_Exit
    )
    (C.G.V.create BB_Entry);
  (* add jumps for edges that need them *)
  C.G.iter_vertex 
    (fun b -> 
       C.G.iter_succ (fun s -> if not(BH.mem joined b) then ensure_jump b s) c b
    )
    c;
  let revordered_heads, exittrace =
    BH.fold
      (fun h t (rh,et) ->
	 if C.G.V.label h = BB_Entry then (rh,et)
	 else if C.G.V.label t = BB_Exit then (rh, Some h)
	 else (h::rh, et) )
      tails
      ([C.G.V.create BB_Entry], None)
  in
  let revordered_heads = match exittrace with
    | Some x -> x::revordered_heads
    | None ->
	if C.G.mem_vertex c (C.G.V.create BB_Exit)
	then failwith "brokenness: BB_Exit was missing"
	else revordered_heads
  in
  let revnodes =
    let rec head_to_revnodes h acc =
      match bh_find_option joined h with
      | Some s -> head_to_revnodes s (h::acc)
      | None -> (h::acc)
    in
    List.fold_right head_to_revnodes revordered_heads []
  in
  let add_stmts stmts b =
    dprintf "to_prog: Adding statements for %s" (v2s b);
    let stmts = List.rev_append (get_revstmts b) stmts in
    try Label(BH.find newlabs b, []) :: stmts with Not_found -> stmts
  in
  List.fold_left add_stmts [] revnodes

