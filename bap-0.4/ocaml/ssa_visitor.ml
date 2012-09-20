(* Visitor for SSA *)

open Type
open Ssa



class type t = object
  (** Called when visiting an expression *)
  method visit_exp: exp -> exp visit_action

  (** Called when visiting a statement *)
  method visit_stmt : stmt -> stmt visit_action

  (** Called when visiting a value *)
  method visit_value : value -> value visit_action

  (** Called when visiting a referenced variable. See also {!visit_avar}. *)
  method visit_rvar : var -> var visit_action

  (** Called when visiting an assigned variable.
      Note that in a Move(), referenced variables will be visited first, so
      that this can be used to add the assigned variable to your context.
  *)
  method visit_avar : var -> var visit_action
end

class nop : t = object
  method visit_exp _   = `DoChildren
  method visit_value _ = `DoChildren
  method visit_stmt _  = `DoChildren
  method visit_avar _  = `DoChildren
  method visit_rvar _  = `DoChildren
end


let rec action vischil startvisit node=
  match startvisit node with
  | `SkipChildren -> node
  | `ChangeTo x -> x (* FIXME: warn if x = node *)
  | `DoChildren -> vischil node
  | `ChangeToAndDoChildren x -> vischil x
  | _ -> failwith "Action not implemented"
(*  | `DoChildrenPost f -> f (vischil node)
  | `ChangeDoChildrenPost(x,f) -> f (vischil x)*)
(*  | `Combine(a,b) ->
      let r1 =  action vischil (fun _ -> a) node in
      action vischil (fun _ -> b) r1
  | `AfterChildren f ->
      action vischil f (vischil node)
*)

let wrapstmt f v = let v' = f v in if quick_stmt_eq v v' then v else v'
let wrapexp f v = let v' = f v in if quick_exp_eq v v' then v else v'
let wrapval f v = let v' = f v in if quick_value_eq v v' then v else v'

let id x = x

let rec exp_accept visitor = 
  let vischil = function
    | Ite(cond, v1, v2) ->
	let vc' = value_accept visitor cond in 
	let v1' = value_accept visitor v1 in 
	let v2' = value_accept visitor v2 in 
	Ite(vc', v1', v2')
    | Extract(h, l, v) ->
	let v' = value_accept visitor v in
	Extract(h, l, v')
    | Concat(lv, rv) ->
	let lv' = value_accept visitor lv in
	let rv' = value_accept visitor rv in
	Concat(lv', rv')
    | BinOp(bop, v1, v2) -> 
	let v1' = value_accept visitor v1 in 
	let v2' = value_accept visitor v2 in 
	BinOp(bop, v1', v2')
    | UnOp(up, v) -> 
	let v' = value_accept visitor v in 
	UnOp(up, v')
    | Val(v) -> 
	let v' = value_accept visitor v in 
	Val(v')
    | Cast(ct, t, v) ->
	let v' = value_accept visitor v in 
	Cast(ct,t,v')
    | Unknown _ as exp -> exp
    | Load(v1,v2,v3, t) -> 
	let v1' = value_accept visitor v1 in 
	let v2' = value_accept visitor v2 in 
	let v3' = value_accept visitor v3 in 
	Load(v1',v2',v3', t)
    | Store(v1,v2,v3,v4, t) ->
	let v1' = value_accept visitor v1 in 
	let v2' = value_accept visitor v2 in 
	let v3' = value_accept visitor v3 in 
	let v4' = value_accept visitor v4 in 
	Store(v1',v2',v3',v4',t)
    | Phi(vl) ->
	let vl' = List.map (rvar_accept visitor) vl in  
	Phi(vl')
  in
  action (wrapexp vischil) (visitor#visit_exp)


and avar_accept visitor =
  action id (visitor#visit_avar)
and rvar_accept visitor = 
  action id (visitor#visit_rvar)

and value_accept visitor =
  let vischil = function
    | Var var -> Var(rvar_accept visitor var)
    | v -> v
  in
  action (wrapval vischil) (visitor#visit_value)

and stmt_accept visitor = 
  let vischil = function 
      (* TODO: attributes? *)
    | Jmp(l, a) -> Jmp(value_accept visitor l, a) 
    | CJmp(c, l1, l2, a) -> 
	let c' = value_accept visitor c in
	let l1' = value_accept visitor l1 in
	let l2' = value_accept visitor l2 in
	CJmp(c', l1', l2', a)
    | Move(lv, e, a) ->
	let e = exp_accept visitor e in
	let lv = avar_accept visitor lv in
	Move(lv, e, a)
    | Label _ as s -> s
    | Comment _ as s-> s
    | Assert(e,a) -> Assert(value_accept visitor e, a)
    | Halt(e,a) -> Halt(value_accept visitor e, a)
  in
  action (wrapstmt vischil) (visitor#visit_stmt)

let stmts_accept vis stmts =
  List.map (stmt_accept vis) stmts

let prog_accept vis p =
  Cfg.SSA.G.fold_vertex
    (fun abb g ->
       let oldstmts = Cfg.SSA.get_stmts g abb in
       let newstmts = stmts_accept vis oldstmts in
       Cfg.SSA.set_stmts g abb newstmts) p p
