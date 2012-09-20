(** Hacks *)

open Type
open Ast
open Util

module D = Debug.Make(struct let name = "Hacks" and default=`Debug end)
open D

let ra_final = Var.newvar "ra_final" reg_32
and ra0 = Var.newvar "ra0" Ast.reg_32
and (mem,sp,r_of) =
  let d = Asmir.decls_for_arch Asmir.arch_i386 in
  (List.hd d,
   List.find (fun v -> Var.name v = "R_ESP") d,
   List.find (fun v -> Var.name v = "R_OF") d
  )

let function_end = "function_end"
and attrs = [StrAttr "ret hack"]
let save_ra0 = Move(ra0, Load(Var mem, Var sp, exp_false, reg_32), attrs)

let ret_to_jmp ?(ra=ra_final) p =
  let a = Array.of_list p in
  Array.iteri
    (fun i s -> match s with
      (* Old lifting of ret *)
     | Special("ret", _) ->
	 a.(i) <- Jmp(Lab function_end, attrs);
	 (match a.(i-1) with 
	  | Jmp(t,at) -> a.(i-1) <- Move(ra, t, attrs@at)
	  | _ -> failwith "expected Jmp before ret special"
	 )
     (* disasm_i386 lifting of ret *)
     | Jmp(t, attrs) when attrs = [StrAttr "ret"] ->
       a.(i) <- Jmp(Lab function_end, attrs)
     | _ -> ()
    ) a;
  let l = Array.to_list a in
  save_ra0::l@[Label(Name function_end, attrs)]


let attrs = [StrAttr "noof hack"]
let assert_noof p =
  let il = List.map
    (function
       | Move(v, e, a) as s when v == r_of ->
	   [s; Assert(exp_not (Var v), attrs)]
       | s -> [s]
    ) p
  in
  List.flatten il
	   

let remove_backedges cfg =
  let module C = Cfg.AST in
  let a = [StrAttr "added by remove_backedeges"] in
  let assert_false = Assert(exp_false, a) in
  let cfg, error = Cfg_ast.find_error cfg in
  let handle_backedge cfg e =
    let s = C.G.E.src e in
    let revstmts = List.rev (C.get_stmts cfg s) in
    let revstmts = match revstmts with
      | Jmp(t,_)::rest ->
	  assert_false::rest
      | CJmp(c,t1,t2,attrs)::rest ->
	(* e is the label we are REMOVING *)
	  let (t,c) = match C.G.E.label e with
	    | Some true -> (t2, exp_not c)
	    | Some false -> (t1, c)
	    | None -> failwith "missing edge label from cjmp"
	  in
	Jmp(t, attrs)::Assert(c,a)::rest
      | rest -> assert_false::rest
    in
    let cfg = C.set_stmts cfg s (List.rev revstmts) in
    let cfg = C.remove_edge_e cfg e in
    if C.G.succ cfg s = [] then C.add_edge cfg s error else cfg
  in
  let find_backedges cfg =
    let module H = Hashtbl.Make(Cfg.BBid) in
    let h = H.create (C.G.nb_vertex cfg)
    and entry = C.find_vertex cfg Cfg.BB_Entry in
    let color v = try H.find h (C.G.V.label v) with Not_found -> false
    and setcolor v c = H.replace h (C.G.V.label v) c in
    let rec walk v edges=
      let walk_edge e edges =
	let d = C.G.E.dst e in
	if color d then e::edges
	else walk d edges
      in
      setcolor v true;
      let edges = C.G.fold_succ_e walk_edge cfg v edges in
      setcolor v false;
      edges
    in
    walk entry []
  in
  let backedges = find_backedges cfg in
  (* SUPER HACK ALERT XXXXXXXX. Fix the above algorithm; don't use two
     colors *)
  let backedges = Util.list_unique backedges in
  List.fold_left handle_backedge cfg backedges
