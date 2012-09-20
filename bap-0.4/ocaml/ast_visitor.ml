(** Visitor for BAP AST

    The design of this visitor was highly influenced by the one from CIL.
 *)

open Type
open Ast
open BatListFull


class type t = object
  (** Called when visiting an expression *)
  method visit_exp: exp -> exp visit_action

  (** Called when visiting a statement *)
  method visit_stmt : stmt -> stmt visit_action

  (** Called when visiting a referenced variable *)
  method visit_rvar : var -> var visit_action

  (** Called when visiting assigned variable.

      Note that in a Move() or Let(), referenced variables will be
      visited first, so that this can be used to add the assigned
      variable to your context.
  *)
  method visit_avar : var -> var visit_action

  (** Called when visiting a variable being unbound.  For instance,
      variable x after visiting let x = y in z. *)
  method visit_uvar: var -> var visit_action

  (** Called on the binding when recursing into a Let. This allows
      doing stuff between the first and second expressions in a Let. *)
  method visit_binding: var * exp -> (var * exp) visit_action


end

class nop : t = object
  method visit_exp _   = `DoChildren
  method visit_stmt _  = `DoChildren
  method visit_avar _  = `DoChildren
  method visit_rvar _  = `DoChildren
  method visit_binding _ = `DoChildren
  method visit_uvar _ = `DoChildren
end


let rec action vischil startvisit node=
  match startvisit node with
  | `SkipChildren -> node
  | `ChangeTo x -> x (* FIXME: warn if x = node *)
  | `DoChildren -> vischil node
  | `ChangeToAndDoChildren x -> vischil x

let wrapstmt f v = let v' = f v in if quick_stmt_eq v v' then v else v'
let wrapexp f v = let v' = f v in if quick_exp_eq v v' then v else v'

let rec exp_accept visitor =
  let vischil = function
    | Int _ as i -> i
    | Lab _ as l -> l
    | Var v ->
	Var(rvar_accept visitor v)
    | Ite(b, v1, v2) ->
	let b' = exp_accept visitor b in
	let v1' = exp_accept visitor v1 in
	let v2' = exp_accept visitor v2 in
	Ite(b', v1', v2')
    | Extract(h, l, v) ->
	let v' = exp_accept visitor v in
	Extract(h, l, v')
    | Concat(vl, vr) ->
	let vl' = exp_accept visitor vl in
	let vr' = exp_accept visitor vr in
	Concat(vl', vr')
    | BinOp(bop, v1, v2) -> 
	let v1' = exp_accept visitor v1 in 
	let v2' = exp_accept visitor v2 in 
	BinOp(bop, v1', v2')
    | UnOp(up, v) -> 
	let v' = exp_accept visitor v in 
	UnOp(up, v')
    | Cast(ct, t, v) ->
      let v' = exp_accept visitor v in
      Cast(ct,t,v')
    | Unknown _ as exp -> exp
    | Load(v1,v2,v3, t) ->
      let v1' = exp_accept visitor v1 in
      let v2' = exp_accept visitor v2 in
      let v3' = exp_accept visitor v3 in
      Load(v1',v2',v3', t)
    | Store(v1,v2,v3,v4, t) ->
      let v1' = exp_accept visitor v1 in
      let v2' = exp_accept visitor v2 in
      let v3' = exp_accept visitor v3 in
      let v4' = exp_accept visitor v4 in
      Store(v1',v2',v3',v4',t)
    | Let(v,e1,e2) ->
      let (v',e1') = binding_accept visitor (v,e1) in
      let e2' = exp_accept visitor e2 in
      let v' = uvar_accept visitor v' in
      Let(v', e1', e2')
  in
  action (wrapexp vischil) visitor#visit_exp


and avar_accept visitor =
  action Util.id visitor#visit_avar
and rvar_accept visitor =
  action Util.id visitor#visit_rvar

and binding_accept visitor =
  let vischil (v,e) =
    let e' = exp_accept visitor e in
    let v' = avar_accept visitor v in
    (v', e')
  in
  action vischil visitor#visit_binding

and uvar_accept visitor =
  let vischil v =
    (* We already visited children in the binding, so we pretend we
       have no children here. *)
    v
  in
  action vischil visitor#visit_uvar

and stmt_accept visitor =
  let vischil = function
      (* TODO: attributes? *)
    | Jmp(l, a) -> Jmp(exp_accept visitor l, a)
    | CJmp(c, l1, l2, a) ->
	let c' = exp_accept visitor c in
	let l1' = exp_accept visitor l1 in
	let l2' = exp_accept visitor l2 in
	CJmp(c', l1', l2', a)
    | Move(lv, e, a) ->
	let e = exp_accept visitor e in
	let lv = avar_accept visitor lv in
	Move(lv, e, a)
    | Label _ as s -> s
    | Comment _ as s-> s
    | Assert(e,a) -> Assert(exp_accept visitor e, a)
    | Halt(e,a) -> Halt(exp_accept visitor e, a)
    | Special _ as s -> s
  in
  action (wrapstmt vischil) (visitor#visit_stmt)

and prog_accept visitor prog =
  List.map (fun instmt -> stmt_accept visitor instmt) prog

and cfg_accept vis p =
  Cfg.AST.G.fold_vertex
    (fun abb g ->
       let oldstmts = Cfg.AST.get_stmts g abb in
       let newstmts = prog_accept vis oldstmts in
       Cfg.AST.set_stmts g abb newstmts) p p
