(** Misc optimizations *)

open Ssa
module C = Cfg.SSA
module D = Debug.Make(struct let name = "SSA_simp_misc" and default=`Debug end)
open D

(** Look for conditional jumps that have a constant as the expression and replace with a jump *)
let cfg_jumpelim graph =
  let changed = ref false in
  let g = C.G.fold_vertex
    (fun bb graph ->
      let stmts = C.get_stmts graph bb in
      if List.length stmts > 0 then (
	let revstmts = List.rev stmts in
	let laststmt = List.hd revstmts in
	match laststmt with
	| CJmp(cond, l1, l2, attr)
	    when full_value_eq cond val_true or full_value_eq cond val_false ->
          changed := true;
	      let (l1, l2) = if full_value_eq cond val_true
		then (l1, l2) else (l2, l1) in
	      let toremove = match val_of_exp l2 with
		| Some(l) -> C.find_label graph l
		| None -> failwith "Unable to convert expression to label" in
	      let graph = C.remove_edge graph bb toremove in
	      let revnewstmts = Jmp(l1, attr)::(List.tl revstmts) in
	      let newstmts = List.rev revnewstmts in
	      C.set_stmts graph bb newstmts
	| _ -> graph)
      else graph
    )
    graph graph in
  g, !changed

