(** Dijkstra's Guarded Command Language

    Type declarations for the Guarded Command Language, and a function
    to traslate a VinE trace into GCL.
    
    Other generally useful functions that deal with the GCL should be
    added here.  

    @author: Ivan Jager
*)

open Type
open Util
open Ast


module D = Debug.Make(struct let name = "GCL" and default=`Debug end)
open D

type var = Var.t
type exp = Ast.exp


(** A GCL expression.
    [Skip] does nothing.
    [Assign] assigns a value to an lvalue.
    [Seq(a,b)] evaluates [a] and then moves on to [b].
    [Choice(a,b)] 
    [Assume] goes on to the next expression in the sequence if it is true.
    [Assert] doesn't start when the condition is not true.
*)
type t =
  | Assume of exp
  | Assign of var * exp
  | Assert of exp
  | Choice of t * t
  | Seq of t * t
  | Skip 




(** Convert a straightline trace into GCL.

    A straightline trace cannot have any CJmps, and any Jmps it might have must
    jump to the label following them. (Ie, any jump must be a no-op)
*)
let rec of_rev_straightline ?(acc=Skip) trace =
  let rec prepend g = function
    | Move(l,e,_) -> Seq(Assign(l,e), g)
    | CJmp _
    | Jmp _ ->
	invalid_arg "found Jmp in straightline GCL"
    | Comment _
    | Label _ ->
	g
    | Halt _
    | Special _ -> 
	invalid_arg "Found halt or special in straightline code"
    | Ast.Assert(e, _) -> Seq(Assert(e), g) 
  in
  (* fold_left of reversed list, rather than fold_right, because
   * fold_right is not tail recursive *)
  List.fold_left prepend acc trace

let of_straightline trace = of_rev_straightline(List.rev trace)



module CA = Cfg.AST

module RevCFG =
struct
  type t = CA.G.t
  module V = CA.G.V
  let iter_vertex = CA.G.iter_vertex
  let iter_succ = CA.G.iter_pred
  let in_degree = CA.G.out_degree
end

module Toposort = Graph.Topological.Make(RevCFG);;

(* type used internaly by of_cfg *)
type cfg_gcl =
  | CAssign of CA.G.V.t
  | CChoice of exp * cfg_gcl * cfg_gcl (* bb with cjmp, true  and false branches *)
  | Cunchoice of cfg_gcl * cfg_gcl (* unfinished choice *)
  | CSeq of cfg_gcl list



(** [of_cfg cfg exit_node] will compute a function from entry node to 
    gcl between the entry node and the exit node. [cfg] must be acyclic. *)
let of_astcfg ?entry ?exit cfg =
  let exit = match exit with
    | None -> CA.G.V.create Cfg.BB_Exit
    | Some x -> x
  and entry = match entry with
    | None -> CA.G.V.create Cfg.BB_Entry
    | Some x -> x
  in
  (* our latice isa list option of GCL expressions to be put in sequence *)
  let meet l1 l2 =
    let (su, g1, g2) = split_common_suffix l1 l2 in
    Cunchoice(CSeq g1, CSeq g2)  :: su
  in
    (* a skip in this context is a CSeq(CSeq [],..) and the like *)
  let rec remove_skips g = 
    match g with
    | CAssign _ -> g
    | CSeq [] -> g
    | CSeq [x] -> x
    | CSeq sl ->
	(* <@ is the composition operator from bap_util. This
	   recursively goes through sl and calls remove_skips on
	   each list item, along with the filtering. *)
	CSeq(list_filter_some
	       ((function CSeq[] -> None | x -> Some x) <@ remove_skips) sl )
    | CChoice(c,s1,s2) -> CChoice(c, remove_skips s1, remove_skips s2)
    | Cunchoice(s1,s2) -> Cunchoice(remove_skips s1, remove_skips s2)
  in
    (* finds first assignment *)
  let rec find_first s =
    match remove_skips s with
    | CAssign b -> b
    | CSeq(h::_) -> find_first h
    | CSeq [] -> failwith "shouldn't ever get here tnh99btcn"
    | CChoice _ -> failwith "shouldn't ever get here tnh9rh203"
    | Cunchoice _ -> failwith "shouldn't ever get here tnh982h9o"
  in
    (* find cjmp in a block. assumes it is the last statement in the
       block *)
  let rec find_cjmp = function
    | [] -> None
    | stmts ->
	match list_last stmts with
	| CJmp(c,t,f,_) -> Some(c,t,f)
	| s -> None
  in
  let find_target e =
    match lab_of_exp e with
    | Some l -> CA.find_label cfg l
    | _ -> failwith "indirect jump not supported yet"
  in
  let b_to_string b = Cfg.bbid_to_string (CA.G.V.label b) in
  (* transfer function *)
  let f_t n = function
    | Cunchoice(bb1, bb2)::rest as exp -> 
	(match find_cjmp (CA.get_stmts cfg n) with
	 | Some(cond,tt,ft) ->
	     let (bbt,bbf) =
	       match (find_first (CSeq(bb1::rest)), find_first (CSeq(bb2::rest)),
		      find_target tt, find_target ft) with
		 (b1,b2,bt,bf) when b1 = bt && b2 = bf -> (bb1,bb2)
	       | (b1,b2,bt,bf) when b2 = bt && b1 = bf -> (bb2,bb1)
	       | (b1,b2,bt,bf) ->
		   failwith(Printf.sprintf "choice seems to not correspond to cjmp %s %s %s %s at %s"
			      (b_to_string b1) (b_to_string b2)
			      (b_to_string bt) (b_to_string bf)
			      (b_to_string n) )
	     in
	     CAssign n::CChoice(cond, bbt, bbf)::rest
	 | None -> (* No CJmp found *)
	     dprintf "Warning: CJmp expected but not found at end of %s." (b_to_string n);
	     CAssign n::exp
	)
    | exp -> CAssign n::exp
  in
  let rec cgcl_to_gcl = function
    | CChoice(cond, e1, e2) ->
	Choice(Seq(Assume cond, cgcl_to_gcl e1),
	       Seq(Assume(exp_not cond), cgcl_to_gcl e2) )
    | Cunchoice(e1, e2) ->
	pwarn "generating an unguarded choice";
	Choice(cgcl_to_gcl e1, cgcl_to_gcl e2)
    | CSeq(e'::es) ->
	List.fold_left (fun a b -> Seq(a,cgcl_to_gcl b)) (cgcl_to_gcl e') es
    | CSeq [] ->
	Skip
    | CAssign b -> 
	let bb_s = CA.get_stmts cfg b in
	match List.rev bb_s with
	| [] -> Skip
	| (Jmp _ | CJmp _)::rest -> of_rev_straightline rest
	| _ -> of_straightline bb_s
  in

  let module BH = Hashtbl.Make(CA.G.V) in
  let h = BH.create (CA.G.nb_vertex cfg) in
  let get b =
    try BH.find h b
    with Not_found -> failwith("no GCL at "^b_to_string b)
  in
  (*BH.add h exit []; *)
  let compute_at b =
    let last_gcl = match CA.G.succ cfg b with
      | [p] -> get p
      | [x;y] -> meet (get x) (get y)
      | s when CA.G.V.equal exit b -> assert(s=[]); []
      | [] -> (* can never reach exit from here *)
	  assert(CA.G.V.label b = Cfg.BB_Error);
	  [CAssign b] (* BB_Error should contain an assert(false) *)
      | _ -> failwith("indirect jmp unsupported. "^b_to_string b^" had too many successors")
    in
    let gcl = f_t b last_gcl in
    BH.add h b gcl
  in
  Toposort.iter compute_at cfg;
  cgcl_to_gcl (CSeq(get entry))


let of_ast p =
  of_astcfg (Prune_unreachable.prune_unreachable_ast (Cfg_ast.of_prog p))


let rec remove_skips = function
  | Assume _
  | Assert _
  | Assign _
  | Skip as gcl -> gcl
  | Choice(g1,g2) -> (
      match (remove_skips g1 , remove_skips g2) with
      | (Skip, Skip) -> Skip
      | (a, b) -> Choice(a,b)
    )
  | Seq(g1,g2) -> (
      match (remove_skips g1 , remove_skips g2) with
      | Skip, Skip -> Skip
      | Skip, x -> x
      | x, Skip -> x
      | x, y -> Seq(x,y)
    )


module C = Cfg.SSA

let passified_of_ssa ?entry ?exit cfg =
  let ast = Cfg_ssa.to_astcfg ~dsa:true cfg in
  let convert = function
    | Some v -> Some(CA.find_vertex ast (C.G.V.label v))
    | None -> None
  in
  let entry = convert entry and exit = convert exit in
  let gcl = of_astcfg ?entry ?exit ast in
  let vars = ref [] in
  let rec convert_gcl g = 
    match g with
    | Assign(v,e) ->
	vars := v :: !vars;
	Assume(exp_eq (Var v) e)
    | Choice(a,b) ->
	Choice(convert_gcl a, convert_gcl b)
    | Seq(a,b) ->
	Seq(convert_gcl a, convert_gcl b)
    | Assume _ | Assert _ | Skip ->
	g
  in
  let pgcl = convert_gcl gcl in
  (pgcl, list_unique !vars)


let passified_of_astcfg ?entry ?exit cfg =
  let {Cfg_ssa.cfg=ssa; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let convert = function
    | Some v -> Some(C.find_vertex ssa (CA.G.V.label v))
    | None -> None
  in
  let entry = convert entry and exit = convert exit in
  let (g,v) = passified_of_ssa ?entry ?exit ssa in
  (g,v,tossa)
