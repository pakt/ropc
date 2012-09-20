(* 
    Static Single Assignment translation

    @author Ivan Jager
*)

open Big_int
open Big_int_convenience
open Util
open Ssa
open Cfg
open Type
open BatListFull

module D = Debug.Make(struct let name = "SSA" and default=`Debug end)
open D

module VH = Var.VarHash
module C = Cfg.SSA
module CA = Cfg.AST
module Dom = Dominator.Make(C.G)

let v2s n = bbid_to_string (C.G.V.label n)

(* A translation context (for translating to SSA) *)
module Ctx =
struct
  type t = var VH.t * var VH.t * (var*var) Stack.t Stack.t
  let create() = (VH.create 570, VH.create 570, Stack.create())
  let lookup (vh,_,_) var =
    try VH.find vh var
    with Not_found -> var
      
  let extend (vh,to_oldvar,stacks) v v' =
    Stack.push (v,v') (Stack.top stacks);
    VH.add vh v v';
    VH.add to_oldvar v' v

  (* Called to add a let variable to the context *)
  let letextend (vh,to_oldvar,_) v v' =
    VH.add vh v v'
      (* FIXME: We didn't used to add these to to_oldvar, do we want to now? *)
      (* VH.add to_oldvar v' v *)

  (* Called to remove a let variable from the context *)
  let letunextend (vh,_,_) v =
    VH.remove vh v

  let push (_,_,stacks) =
    Stack.push (Stack.create()) stacks

  let pop (vh,_,stacks) =
    let myvars = Stack.pop stacks in
    Stack.iter (fun (v,_) -> VH.remove vh v) myvars
end

(* This should probably be somewhere else... *)
let type_of_value = function
  | Int(_,t) -> t
  | Var v -> Var.typ v
  | Lab _ -> failwith "arg blarg, need types for labels"
let type_of_exp = function
  | Load(_,_,_,t)
  | Cast(_,t,_)
  | Unknown(_,t)
    -> t
  | BinOp((EQ|NEQ|LT|LE|SLT|SLE),_,_)
    -> Ast.reg_1
  | Ite(_,v,_)
  | BinOp(_,v,_)
  | Store(v,_,_,_,_)
  | UnOp(_,v)
  | Val v
    -> type_of_value v
  | Extract(h, l, v) ->
      let n = ((h -% l) +% bi1) in
      assert(n >=% bi1);
      Reg(int_of_big_int n)
  | Concat(lv, rv) ->
      (match type_of_value lv, type_of_value rv with
      | Reg(lt), Reg(rt) -> Reg(lt + rt)
      | _ -> failwith "type_of_exp")
  | Phi(x::_)
    -> Var.typ x
  | Phi []
    -> failwith "Empty phi has no type"




(* share the strings in the variable names we create, to save memory *)
let ssa_temp_name = "temp"

(* @return a reversed lits of SSA stmts and an exp that is equivalent to
   the Ast expression *)
let rec exp2ssaexp (ctx:Ctx.t) ~(revstmts:stmt list) ?(attrs=[]) e : stmt list * exp =
  match e with 
  | Ast.Ite(b, e1, e2) ->
      let (revstmts, vb) = exp2ssa ctx revstmts b in
      let (revstmts, v1) = exp2ssa ctx revstmts e1 in
      let (revstmts, v2) = exp2ssa ctx revstmts e2 in
      (revstmts, Ite(vb, v1, v2))
  | Ast.Extract(h, l, e) ->
      let (revstmts, ve) = exp2ssa ctx revstmts e in
      (revstmts, Extract(h, l, ve))
  | Ast.Concat(le, re) ->
      let (revstmts, lv) = exp2ssa ctx revstmts le in
      let (revstmts, rv) = exp2ssa ctx revstmts re in
      (revstmts, Concat(lv, rv))
  | Ast.BinOp(op, e1, e2) -> 
      let (revstmts, v1) = exp2ssa ctx revstmts e1 in
      let (revstmts, v2) = exp2ssa ctx revstmts e2 in
      (revstmts, BinOp(op,v1,v2))
  | Ast.UnOp(op, e1) ->
      let (revstmts, v1) = exp2ssa ctx revstmts e1 in
      (revstmts, UnOp(op,v1))
  | Ast.Int(i, t) ->
      (revstmts, Val(Int(i,t)))
  | Ast.Lab s ->
      (revstmts, Val(Lab s))
  | Ast.Var name -> 
      (revstmts, Val(Var(Ctx.lookup ctx name)))
  | Ast.Load(arr,idx,endian, t) ->
      let (revstmts,arr) = exp2ssa ctx revstmts arr in
      let (revstmts,idx) = exp2ssa ctx revstmts idx in
      let (revstmts,endian) = exp2ssa ctx revstmts endian in
      (revstmts, Load(arr,idx,endian,t))
  | Ast.Store(arr,idx,vl, endian, t) ->
      let (revstmts,arr) = exp2ssa ctx revstmts arr in
      let (revstmts,idx) = exp2ssa ctx revstmts idx in
      let (revstmts,vl) = exp2ssa ctx revstmts vl in
      let (revstmts,endian) = exp2ssa ctx revstmts endian in
      (revstmts, Store(arr,idx,vl, endian,t))
  | Ast.Cast(ct,t,e1) ->
      let (revstmts, v1) = exp2ssa ctx revstmts e1 in
      (revstmts, Cast(ct,t,v1))
  | Ast.Unknown(s,t) ->
      (revstmts, Unknown(s,t))
  | Ast.Let(v, e1, e2) ->
      let v' = Var.renewvar v in
      let (revstmts,e1) = exp2ssaexp ctx revstmts e1 in
      let revstmts = Move(v',e1, attrs)::revstmts in
      Ctx.letextend ctx v v';
      let (revstmts,e2) = exp2ssaexp ctx revstmts e2 in
      Ctx.letunextend ctx v;
      (revstmts, e2)


(* @return a reversed lits of SSA stmts and a value that is equivalent to
   the Ast expression *)
and exp2ssa ctx ~(revstmts:stmt list) ?(attrs=[]) ?(name=ssa_temp_name) e : stmt list * value =
  (* Make an SSA value for an SSA expression by adding an assignment to
     revstmts if needed *)
  let exp2val (revstmts, exp) =
    match exp with
    | Val v -> (revstmts, v)
    | _ ->
	let t = type_of_exp exp in
	let l = Var.newvar name t in
	(Move(l, exp, attrs)::revstmts, Var l)
  in
  exp2val(exp2ssaexp ctx revstmts e)


(* @return a reversed list of SSA stmts *)
let rec stmt2ssa ctx ~(revstmts: stmt list) s =
  match s with
      Ast.Jmp(e1, a) ->
	let (revstmts,v1) = exp2ssa ctx revstmts e1 in
	  Jmp(v1, a) :: revstmts
    | Ast.CJmp(e1,e2,e3,a) ->
	let (revstmts,v1) = exp2ssa ctx revstmts e1 in
	let (revstmts,v2) = exp2ssa ctx revstmts e2 in
	let (revstmts,v3) = exp2ssa ctx revstmts e3 in
	  CJmp(v1,v2,v3,a) :: revstmts
    | Ast.Move(v, e2, a) ->
	let (revstmts, e) = exp2ssaexp ctx revstmts e2 in
	let nv = Var.renewvar v in
	Ctx.extend ctx v nv;
	Move(nv, e, a)::revstmts
    | Ast.Label(label,a) ->
	Label(label,a) :: revstmts
    | Ast.Comment(s,a) ->
	Comment(s,a)::revstmts
    | Ast.Special(s,_) -> 
	raise (Invalid_argument("SSA: Impossible to handle specials. They should be replaced with their semantics. Special: "^s))
    | Ast.Assert(e,a) ->
	let (revstmts,v) = exp2ssa ctx revstmts e in
	  Assert(v,a)::revstmts
    | Ast.Halt(e,a) ->
	let (revstmts, v) = exp2ssa ctx revstmts e in 
	  Halt(v,a)::revstmts
	

(* Translates a list of Ast statements that get executed sequentially to SSA. *)
let stmts2ssa ctx ss =
  let revstmts = List.fold_left (fun rs s -> stmt2ssa ctx rs s) [] ss in
    List.rev revstmts


(* This is only for use by trans_cfg, as it has some hacks *)
let defsites cfg =
  let h = VH.create 57
  and globals = ref [] in
  let defs stmts =
    let res = ref [] in
    let f = function
	| Ast.Move(v, _, _) ->  res := v :: !res; globals := v :: !globals
	| _ -> ()
    in
    List.iter f stmts;
    !res
  in
  CA.G.iter_vertex 
    (fun b ->
       let id = CA.G.V.label b in
       let vars = list_unique  (defs (CA.get_stmts cfg b)) in
       List.iter (fun v -> VH.add h v id) vars
    )
    cfg;
  (* add globals as being defined at the entry node. We only actually need
     the globals that might conditionally be assigned to. *)
  List.iter (fun v -> VH.add h v BB_Entry) (list_unique !globals);
  (VH.find_all h, !globals)
    (* FIXME: maybe avoiding find_all will make it faster *)


type translation_results = {
  cfg : Cfg.SSA.G.t;
  to_astvar: Var.t -> Var.t; (* Maps SSA vars back to the variable they came from *)
  to_ssavar: Var.t -> Var.t; (* Maps AST vars to SSA at end of exit node. *)
}  

(** Translates a CFG into SSA form.
    Returns the new SSA CFG and two maps. One from SSA variables to the
    variables they originally came from, and the other from the original
    variables to what they map to at the end of the exit node. Both raise
    Not_found for variables that don't map to anything. (Eg, for temporary
    variables introduced by SSA, or variables that weren't assigned.)
 *)
let rec trans_cfg cfg =
  pdebug "Translating to SSA";
  (* if debug && not(Ast_cfg.well_defined cfg) then
    raise(TypeError "Ssa.trans_cfg: given cfg not well defined");*)

  let cfg = Prune_unreachable.prune_unreachable_ast (CA.copy cfg) in
  pdebug "Creating new cfg";
  let ssa = Cfg.map_ast2ssa (fun _ -> []) cfg in
  pdebug "Computing defsites";
  let (defsites, globals) = defsites cfg in
    (* keep track of where we need to insert phis *)
  let phis : (bbid * var, var * var list) Hashtbl.t = Hashtbl.create 57 in
  pdebug "Computing dominators";
  let {Dom.dom_tree=dom_tree; Dom.dom_frontier=df} =
    Dom.compute_all ssa (C.G.V.create BB_Entry)
  in
  let add_phis_for_var v =
    (* Note that contrary to the book, we don't need membership testing at all
       for the worklist, since the only time we try to add a node is when it
       couldn't be in the worklist in the first place. --aij
       Errata sent to Andrew W. Appel <appel@princeton.edu> on 2007-06-10
    *)
    (* let () = dprintf "Adding phis for variable '%s'" (var_to_string v) in *)
    let rec do_work = function
      | [] -> ()
      | n::worklist ->
	  let worklist =
	    List.fold_left
	      (fun toadd y ->
		 let y = C.G.V.label y in (* for now *)
		 if not(Hashtbl.mem phis (y,v))
		 then (Hashtbl.add phis (y,v) (v,[]);
		       if List.mem y (defsites v) then toadd else y::toadd )
		 else toadd
	      )
	      worklist
	      (df (C.G.V.create n))
	  in
	    do_work worklist
    in
      do_work (defsites v)
  in
  dprintf "Adding phis";
  List.iter add_phis_for_var globals;
  dprintf "Added %d phis" (Hashtbl.length phis);
    (* we now have an entry in phis for every phi expression
       we need to add, although we still don't have the RHS and LHS. *)
  dprintf "Grouping phis by block";
  let blockphis =
    (* returns the phis for a given block *)
    let h = Hashtbl.create 57 in
    Hashtbl.iter (fun (n,v) _ -> Hashtbl.add h n v) phis;
    Hashtbl.find_all h
  in
  let exitctx = VH.create 57 in (* context at end of exit node *)
  let (vh_ctx,to_oldvar,stacks) as ctx = Ctx.create() in
  let lookup = Ctx.lookup ctx in
  let extend = Ctx.extend ctx in
  let rec rename_block ssa b =
    let bbid = C.G.V.label b in
    dprintf "Translating block %s" (bbid_to_string bbid);
    let cfgb = CA.G.V.create bbid in
    Ctx.push ctx;
    let () =
      (* create variables for our phis *)
      List.iter
	(fun v ->
	   let v' = Var.renewvar v in
	   let (v'',vs) = Hashtbl.find phis (bbid,v) in
	     assert(v'' == v);
	     Hashtbl.replace phis (bbid,v) (v',vs);
	     extend v v'
	)
	(blockphis bbid)
    in
    let ssa = 
      (* rename variables *)
      let stmts = CA.get_stmts cfg cfgb in
      dprintf "translating stmts";
      let stmts' = stmts2ssa ctx stmts in
      C.set_stmts ssa b stmts'
    in
    dprintf "going on to children";
    (* rename children *)
    (* List.iter (fun n -> dprintf "Dominates %s" (v2s n)) (dom_tree b); *)
    let ssa = List.fold_left rename_block ssa (dom_tree b) in
    let () =
      (* Update any phis in our successors *)
      List.iter
	(fun s ->
	   let s = C.G.V.label s in
	   List.iter
	     (fun v ->
		try 
		  let (p,vs) = Hashtbl.find phis (s,v) in
		  let v' = try lookup v with Not_found -> v  in
		    Hashtbl.replace phis (s,v) (p, v'::vs)
		with Not_found ->
		  failwith("phi for variable "^Pp.var_to_string v
			   ^" not found in "^Cfg.bbid_to_string s)
	     )
	     (blockphis s)
	)
	(C.G.succ ssa b)
    in
   (* save context for exit node *)
    (if bbid = BB_Exit then (
      (* dprintf "Exit ctx:"; *)
      (* VH.iter (fun k v -> dprintf "%s -> %s" (Pp.var_to_string k) (Pp.var_to_string v)) vh_ctx; *)
      VH.iter (fun k v -> VH.replace exitctx k (VH.find vh_ctx k)) vh_ctx ));
   (* restore context *)
    Ctx.pop ctx;
    ssa
  in
  let ssa = rename_block ssa (C.G.V.create BB_Entry) in
  dprintf "Adding %d phis to the CFG" (Hashtbl.length phis);
  let rec split_labels revlabels stmts =
    match stmts with
      | ((Label _ | Comment _) as s)::ss ->
	  split_labels (s::revlabels) ss
      | _ -> (revlabels, stmts)
  in
  let ssa =
    (* actually add all our phis to the CFG *)
    C.G.fold_vertex
      (fun b ssa ->
	 let bbid = C.G.V.label b in
	 let vars = blockphis bbid in
	 let (revlabs,stmts) = split_labels [] (C.get_stmts ssa b) in
	 let stmts =
	   List.fold_left
	     (fun s v ->
		let (p,vs) = Hashtbl.find phis (bbid,v) in
		assert(vs <> []);
		(* FIXME: do something reasonable with attributes *)
		Move(p,Phi(vs), [])::s )
	     stmts
	     vars
	 in
	 C.set_stmts ssa b (List.rev_append revlabs stmts)
      )
      ssa ssa
  in
  dprintf "Done translating to SSA";
  (* VH.iter (fun k v -> dprintf "%s -> %s" (Pp.var_to_string k) (Pp.var_to_string v)) exitctx; *)
  {cfg=ssa; to_astvar=VH.find to_oldvar; to_ssavar=VH.find exitctx}

(** Translates a CFG into SSA form. *)
let of_astcfg cfg =
  let {cfg=ssa} = trans_cfg cfg in
  ssa

(** Translates an AST program into an SSA CFG. *)
let of_ast p = 
  of_astcfg (Cfg_ast.of_prog p)


let uninitialized cfg =
  let module VS = Var.VarSet in
  let refd = ref VS.empty
  and assnd = ref VS.empty
  and add sr v = sr := VS.add v !sr in
  let process stmts =
    let rec f_s = function
      | Move(v, e, _) -> add assnd v; f_e e
      | _ -> ()
    and f_e = function
      | Load(v1,v2,v3,_) -> f_v v1; f_v v2; f_v v3
      | Store(v1,v2,v3,v4,_) -> f_v v1; f_v v2; f_v v3; f_v v4
      | Ite(cond,v1,v2) -> f_v cond; f_v v1; f_v v2
      | Extract(_,_,v) -> f_v v
      | Concat(lv,rv) -> f_v lv; f_v rv
      | BinOp(_,v1,v2) -> f_v v1; f_v v2
      | UnOp(_,v)
      | Cast(_,_,v)
      | Val v -> f_v v
      | Phi vs -> List.iter (add refd) vs
      | Unknown _ -> ()
    and f_v = function
      | Var v -> add refd v
      | Int _ | Lab _ -> ()
    in
    List.iter f_s stmts;
  in
  C.G.iter_vertex (fun b -> process (C.get_stmts cfg b)) cfg;
  VS.diff !refd !assnd


(* Hack to make a "unique" label *)
let rec mklabel =
  let r = ref 0
  and n = "autolabel" in
  let has_label c l =
    try ignore(C.find_label c l); true
     with Not_found -> false
  in
  (fun c ->
     let l = n ^ string_of_int !r in
     incr r;
     if has_label c (Name l) then mklabel c else l
  )

(** Perform edge splitting on the CFG.
    After edge splitting, it is guaranteed that every edge has either a unique
    predecessor or a unique successor.
*)
let split_edges c =
  let module E = C.G.E in
  let split_edge c e =
    let s = E.src e and d = E.dst e in
    (* s has multiple successors and d has multiple predecesors *)
    let revs = List.rev (C.get_stmts c s) in
    let cjmp = List.hd revs in
    let (cond,t1,t2,attrs) = match cjmp with
      | CJmp(c, t1, t2, a) -> (c,t1,t2,a)
      | _ -> failwith("Missing cjmp at end of node with multiple successors: "^Pp.ssa_stmt_to_string cjmp)
    in
    let newl = mklabel c in
    let el = E.label e in
    let (t1,t2,tf) = match el with
      | Some true -> (Lab newl, t2, t1)
      | Some false -> (t1, Lab newl, t2)
      | None -> failwith "Unlabeled edges from cjmp"
    in
    let revs = CJmp(cond, t1, t2, attrs) :: List.tl revs in
    let (c,v) = C.create_vertex c [Label(Name newl, [])] in
    let e' = C.G.E.create s el v in
    let c = C.add_edge_e c e' in
    let c = C.add_edge c v d in
    let c = C.remove_edge_e c e in
    C.set_stmts c s (List.rev revs)
    
  in
  let edges_to_split =
    C.G.fold_edges_e
      (fun e es ->
	 if C.G.out_degree c (E.src e) > 1 && C.G.in_degree c (E.dst e) > 1
	 then e::es
	 else es
      )
      c []
  in
  List.fold_left split_edge c edges_to_split


let list_count p =
  List.fold_left (fun a e -> if p e then a+1 else a) 0


(* FIXME: It might be good to have a different type for a CFG with phis removed *)
let rm_phis ?(dsa=false) ?(attrs=[]) cfg =
  let size = C.G.fold_vertex
    (fun b a ->
       a + list_count (function Move _->true | _->false) (C.get_stmts cfg b) )
    cfg 0
  in
  (* maps variables to the BBs where they were defined *)
  let assn = VH.create size in
  let () =
    C.G.iter_vertex
      (fun b ->
	 List.iter
	   (function
	      | Move(v,_, _) -> VH.add assn v b
	      | _->())
	   (C.get_stmts cfg b)
      )
      cfg
  in
  let entry = C.G.V.create BB_Entry in
    (* fake assignments for globals at the entry node *)
  Var.VarSet.iter (fun v -> VH.add assn v entry) (uninitialized cfg);
  (* split edges if needed *)
  let cfg = if dsa then split_edges cfg else cfg in
  let cfg, phis =
    (* Remove all the phis from all the BBs *)
    (* FIXME: make this readable *)
    C.G.fold_vertex
      (fun b (cfg,phis) ->
	 let (ps,revstmts) =
	   List.fold_left
	     (fun (ps,revstmts) -> function
		| Move(l, Phi vs, _) ->
		    dprintf "rm_phis: removing phi for %s"(Pp.var_to_string l);
		    ((l,vs)::ps, revstmts)
		| s ->
		    (ps, s::revstmts)
	     )
	     (phis,[])
	     (C.get_stmts cfg b)
	 in
	 (* Note that the statements in the block are now reversed *)
	 (C.set_stmts cfg b revstmts, ps)
      )
      cfg
      (cfg, [])
  in
  let append_move b l p cfg=
    (* note that since stmts are reversed, we can prepend
       instead of appending. We must still be careful to not put
       assignmenst after a jump. *)
    let move = Move(l,Val(Var p), attrs) in
    C.set_stmts cfg b
      (match C.get_stmts cfg b with
       | (Jmp _ as j)::stmts
       | (CJmp _ as j)::stmts ->
	   j::move::stmts
       | stmts ->
	   move::stmts )
  in
  let cfg =
    if dsa then (
      let idom = Dom.compute_idom cfg entry in
      (* add an assignment for l to the end of v *)
      let dsa_push (l,vars) cfg bb =
	let rec find_var bb = (* walk up idom tree starting at bb *)
	  try List.find (fun v -> bb = (VH.find assn v)) vars
	  with Not_found -> find_var (idom bb)
	in
	let v = find_var bb in
	append_move bb l v cfg
      in
      (* assign the variable the phi assigns at the end of each of it's
	 predecessors *)
      List.fold_left
	(fun cfg ((l, vars) as p) -> 
	   dprintf "rm_phis: adding assignments for %s" (Pp.var_to_string l);
	   List.fold_left (dsa_push p) cfg (C.G.pred cfg (VH.find assn l))
	)
	cfg
	phis
    )
    else (
    (* assingn the variables the phi assigns at the end of each block a variable
       the phi references is assigned. *)
    List.fold_left
      (fun cfg (l, vars) -> 
	 dprintf "rm_phis: adding assignments for %s" (Pp.var_to_string l);
	 List.fold_left (fun cfg p -> append_move (VH.find assn p) l p cfg) cfg vars
      )
      cfg
      phis
    )
  in
  (* put statements back in forward order *)
  C.G.fold_vertex
    (fun b cfg -> C.set_stmts cfg b (List.rev(C.get_stmts cfg b)))
    cfg cfg

type tm = Ssa.exp VH.t

let create_tm c =
  let tm = VH.create 5700 
  and refd = VH.create 5700 in
  let vis = object
    inherit Ssa_visitor.nop
    method visit_rvar v =
      (try
	 if VH.find refd v then
	   VH.remove tm v
	 else VH.replace refd v false
       with Not_found -> VH.add refd v true);
      `DoChildren

    method visit_stmt = function
      | Move(_, Phi _, _) ->
	  `DoChildren
      | Move(v,e,_) ->
	  (* FIXME: should we check whether we introduced this var? *)
          (* FIX: we introduced it if it is named "temp" *)
	  if (try VH.find refd v with Not_found -> true) 
              && (Var.name v == ssa_temp_name)
	  then VH.add tm v e;
	  `DoChildren
      | _ ->
	  `DoChildren
  end in
  C.G.iter_vertex
    (fun b -> ignore(Ssa_visitor.stmts_accept vis (C.get_stmts c b)))
    c;
  tm


let rec value2ast tm = function
  | Int(i,t) -> Ast.Int(i,t)
  | Lab s -> Ast.Lab s
  | Var l -> try exp2ast tm (VH.find tm l) with Not_found -> Ast.Var l

and exp2ast tm =
  let v2a = value2ast tm in
  function
    | Ite(c,v1,v2) -> Ast.Ite(v2a c, v2a v1, v2a v2)
    | Extract(h,l,v) -> Ast.Extract(h, l, v2a v)
    | Concat(lv,rv) -> Ast.Concat(v2a lv, v2a rv)
    | BinOp(bo,v1,v2) -> Ast.BinOp(bo, v2a v1, v2a v2)
    | UnOp(uo, v) -> Ast.UnOp(uo, v2a v)
    | Val v -> v2a v
    | Cast(ct,t,v) -> Ast.Cast(ct, t, v2a v)
    | Unknown(s,t) -> Ast.Unknown(s,t)
    | Load(arr,idx,e, t) -> Ast.Load(v2a arr, v2a idx, v2a e, t)
    | Store(a,i,v, e, t) -> Ast.Store(v2a a, v2a i, v2a v, v2a e, t)
    | Phi _ -> failwith "exp2ast cannot translate Phi expressions"

(* Translates an SSA stmt back to Ast *)
let stmt2ast tm =
  let v2a = value2ast tm in
  function
    | Jmp(t,a) -> Ast.Jmp(v2a t, a)
    | CJmp(c,tt,tf,a) -> Ast.CJmp(v2a c, v2a tt, v2a tf, a)
    | Label(l,a) -> Ast.Label(l,a)
    | Comment(s,a) -> Ast.Comment(s,a)
    | Assert(t,a) -> Ast.Assert(v2a t, a)
    | Halt(t,a) -> Ast.Halt(v2a t, a)
    | Move(l,e,a) -> Ast.Move(l, exp2ast tm e, a)

let stmts2ast tm stmts =
  let is_trash = function
    | Move(l,_,a) when List.mem Liveout a -> false
    | Move(l,_,_) when VH.mem tm l -> true
    | _ -> false
  in
  List.fold_right
    (fun s ast -> if is_trash s then ast else stmt2ast tm s :: ast)
    stmts []

(** Convert an ssa cfg (with phis already removed) back to a ast cfg *)
let cfg2ast tm cfg =
  Cfg.map_ssa2ast (stmts2ast tm) cfg

(** Convert an SSA CFG to an AST CFG. *)
let to_astcfg ?(remove_temps=true) ?(dsa=false) c =
  let tm = if remove_temps then create_tm c else VH.create 1 in
  cfg2ast tm (rm_phis ~dsa:dsa c)

(** Convert an SSA CFG to an AST program. *)
let to_ast ?(remove_temps=true) c =
  Cfg_ast.to_prog (to_astcfg ~remove_temps c)
