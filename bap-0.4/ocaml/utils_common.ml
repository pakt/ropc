(** Functions that are used by utilities and tests *)

open Ast
open Type


(* For solving predicates *)
let rename_astexp f =
  let vis = object
    inherit Ast_visitor.nop
    method visit_rvar v =
      try `ChangeTo(f v)
      with Not_found -> `DoChildren
  end in
  Ast_visitor.exp_accept vis;;


let to_ssagcl cfg post =
  let cfg = Hacks.remove_backedges cfg in
  let cfg = Coalesce.AST_Coalesce.coalesce cfg in
  let {Cfg_ssa.cfg=cfg; to_ssavar=tossa} = Cfg_ssa.trans_cfg cfg in
  let p = rename_astexp tossa post in
  let cfg =
    let vars = Formulap.freevars p in
    Ssa_simp.simp_cfg ~liveout:vars ~usedc:true ~usesccvn:true cfg
  in
  let cfg = Cfg_ssa.to_astcfg cfg in
  let gcl = Gcl.of_astcfg cfg in
  (gcl, p);;


(* For type checking.  Temporary home. *)
let typecheck p =
  let v = object(self)
    inherit Ast_visitor.nop
    method visit_exp e = ignore(Typecheck.infer_ast ~check:true e); `DoChildren
  end 
  in
  ignore(Ast_visitor.prog_accept v p);
  p
