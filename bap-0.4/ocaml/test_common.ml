(** Functions used by BAP library and tests *)

open Big_int
open Big_int_convenience
open OUnit
open Pcre
open Ast

let speclist = [];;

(** STP helpers **)
let stp_path = "../stpwrap/stp/bin/";;
let stp = "stp";;


let check_stp_path file =
  let path = Sys.getenv("PATH") in
  if (pmatch ~pat:stp_path path) then ()
  else (
	if (Sys.file_exists file) 
	then Unix.putenv "PATH" (path^":"^stp_path)
	else skip_if true 
	  ("Skipping test.  Stp is not in PATH and can not find file "^file));;


(** pin helpers **)
let pin_path = "../pin/";;
let pin = "pin";;
let gentrace_path = "../pintraces/obj-ia32/";;
let gentrace = "gentrace.so";;
let pin_out_suffix = "bap-pin-test.out";;

let rec find_pin_out files tag =
  match files with
  | [] -> assert_failure 
	("Could not find a file with suffix "^tag^pin_out_suffix^" Did pin fail?")
  | f::fs -> if (pmatch ~pat:(tag^pin_out_suffix) f) then f else find_pin_out fs tag;;

let check_pin_setup _ =
  (* Only do this if we are running in an x64 environment *)
  let cat_arg = "/proc/sys/kernel/yama/ptrace_scope" in
  let foutput char_stream = 
	(match (Stream.next char_stream) with
	| '0' -> ()
	| _ -> skip_if true
	  (cat_arg^
		 " must contain 0 for pin to work.  As root, please execute $ echo 0 > "
	   ^cat_arg))
  in
  if (Sys.file_exists cat_arg) 
  then assert_command ~foutput ~verbose:true "cat" [cat_arg]
  else ();;


(** General system functions **)
let check_file file =
  if not(Sys.file_exists file) then skip_if true
   ("File "^file^" does not exist; Skipping this test!");;


let mkdir_and_ignore path = try Unix.mkdir path 0o640 with _ -> ();;


let rm_and_ignore path =
  (try if (Sys.is_directory(path)) then Unix.rmdir path with _ -> ());
  try Sys.remove path with _ -> ();;


let rec rm_and_ignore_list paths =
  match paths with
  | [] -> ()
  | p::ps -> rm_and_ignore p; rm_and_ignore_list ps;;


(** Common functions across multipule tests **)
let rec find_fun ?(msg="") ranges name = match ranges with
  | [] -> assert_failure ("Could not find function "^name^msg)
  | (n,s,e)::rs -> if (n = name) then (s,e) else find_fun ~msg rs name;;


let rec find_call prog = 
  match prog with
  | [] -> assert_failure "Could not find a call in the given function"
  | p::ps ->
	match p with
	| Label(Type.Addr(a),attrs) -> 
	  (match attrs with
	  | [Type.Asm(asm)] -> 
		if (pmatch ~pat:"call" asm) then a else find_call ps
	  | _ -> find_call ps)
	| _ -> find_call ps;;


(* Return list of statments between start_addr and end_addr *)
let inject_stmt prog start_addr asm stmt = 
  let rec inject_stmt_k stmts starta asm_str inj_stmt k =
	match stmts with
	| [] -> 
	  assert_failure ("Could not find asmembly instruction "^asm_str^" in main")
	| s::[] -> 
	  assert_failure ("Could not find asmembly instruction "^asm_str^" in main")
	(* Match for the addr and label at the same time *)
	| s::l::ss -> 
	  match starta with
	  | Some(a) -> (match s with
		| Label(Type.Addr(addr),attrs) -> 
		    if (addr = a) then inject_stmt_k ss None asm_str inj_stmt (l::s::k)
			else inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
		| _ -> inject_stmt_k (l::ss) starta asm_str inj_stmt (s::k)
	  )
	  (* We are inside the desired block; find asm_str and inject inj_stmt *)
	  | None -> (match s with
		| Label(Type.Addr(addr),attrs) -> 
		  (match attrs with
		  | [Type.Asm(asm)] ->
			if (pmatch ~pat:"ret" asm) then (List.rev k)@(s::l::inj_stmt::ss)
			else inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
		  | _ -> inject_stmt_k ss starta asm_str inj_stmt (l::s::k)
		  )
		| _ -> inject_stmt_k (l::ss) starta asm_str inj_stmt (s::k)
	  )
  in
  inject_stmt_k prog (Some(start_addr)) asm stmt [];;


let halt_stmt = Halt(exp_true,[]);;


let check_bigint_answer e correct = 
  match e with
  | Int(int,_) -> if (int <>% correct)
	then 
	  assert_failure 
		("Final value in EAX " ^ (string_of_big_int int) 
		 ^ " does not equal correct value "
		^ (string_of_big_int correct))
	else ()
  | _ -> assert_failure ("Final value in EAX is not an Ast.Int!");;


let check_eax ctx eax =
  let pat = "R_EAX_" in
  Var.VarHash.iter 
	(fun k v ->
	  match k,v with
	  | var,Symbeval.Symbolic e ->
		if (pmatch ~pat (Pp.var_to_string var)) 
		then check_bigint_answer e eax
		else ()
	  | _ -> ()
    ) ctx.Symbeval.delta;;


let check_functions msg ranges names =
  ignore(List.map (find_fun ~msg ranges) names);;

let typecheck p = ignore(Utils_common.typecheck p);;
