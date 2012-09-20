(** A module to perform AST-evaluation.

TODO:
* Symbolic evaluation
* Handle memory correctly.
* Avoid deconstructing and rebuilding expressions in FSE
* Halt would probably be better as an exception.
*)

open Ast
open Big_int
open Big_int_convenience
open Type
open Util

module VH = Var.VarHash

module D = Debug.Make(struct let name = "Eval" and default=`Debug end)
open D


(* Some useful types *)
type addr = int
type instr = stmt
type varval = Ast.exp (* Int of int | Mem of mem | Str of string | Unit *)
type label_kind = label

type ctx = {
  delta: varval VH.t;
  sigma: (addr, instr) Hashtbl.t;
  lambda: (label_kind, addr) Hashtbl.t;
  pc: addr;
  halt: bool
}

type symbolic =
  | Exp of Ast.exp
  | Cond of Ast.exp * Ast.exp * Ast.exp
exception Symbolic of symbolic
exception Uninitialized of Var.t

(* Lookup functions for basic contexts *)
let inst_fetch sigma pc =
  try Hashtbl.find sigma pc 
  with Not_found -> failwith "jump out of range (pc not in dom(Sigma))"

let label_decode lambda lab =
  try Hashtbl.find lambda lab
  with Not_found -> failwith "jump to inexistent label"

let lookup_var delta var =
  try VH.find delta var
  with Not_found -> raise(Uninitialized var)

let lookup_mem mu index =
  try Hashtbl.find mu index 
  with Not_found -> failwith "uninitialized memory"

(* Context update *)
let context_update = VH.replace
let context_copy = VH.copy

let mu_update = Hashtbl.replace

(* Context Contents *)

let print_values delta =
  Printf.printf "values:\n" ;
  Hashtbl.iter 
    (fun k v ->
       match k,v with 
       | var,Int (n,_) -> Printf.printf "%s = %s\n" (Var.name var) (string_of_big_int n)
       | _ -> ()
    ) delta

let print_mem memory =
  Printf.printf "memory:\n" ;
  Hashtbl.iter 
    (fun k v ->
       match k,v with 
       | Int (v,_),Int (n,_) -> Printf.printf "%s -> %s\n" (string_of_big_int v) (string_of_big_int n)
       | _ -> ()
    ) memory


let is_true = (=) exp_true
let is_false = (=) exp_false


let rec eval_expr_symb delta =
  let eval e = eval_expr_symb delta e
  and int (v,t) = Int(v,t) in
  let get e = 
    match eval e with
    | Int (v,t) -> (v,t)
    | e -> raise (Symbolic(Exp e))
  in
  function
    | Var v as e ->
        (try lookup_var delta v
	 with Uninitialized _ -> e )
    | Int _ as value -> 
        value
    | Lab _ as labl ->
        labl
    | BinOp(op,e1,e2) ->
	let e1 = eval e1 and e2 = eval e2 in
	(try int (Arithmetic.binop op (get e1) (get e2))
	 with Symbolic _ ->  BinOp(op, e1, e2) )
    | UnOp(op,e) ->
	let e = eval e in
	(try int (Arithmetic.unop op (get e))
	 with Symbolic _ ->  UnOp(op,e) )
    | Let(var,e1,e2) ->
        let v1 = eval e1 in
	(* FIXME: avoid copying *)
        let delta' = context_copy delta in
        context_update delta' var v1 ;
	eval_expr_symb delta' e2
    | _ ->
	failwith "unimplemented"


(* Evaluate an expression in a context Delta,Mu *)
let rec eval_expr (delta,mu,expr) =
  let get_expr e = 
    match eval_expr (delta,mu,e) with
    | Int (v,t) -> (v,t)
    | _ -> failwith "expression cannot be evaluated"
  in
  let eval = function 
    | Var v -> 
        lookup_var delta v
    | Int _ as value -> 
        value
    | Lab _ as labl ->
        labl
    | Ite(b,e1,e2) ->
	let b = eval_expr (delta,mu,b)
	and v1 = eval_expr (delta,mu,e1)
	and v2 = eval_expr (delta,mu,e2) in
	if is_true b then v1 else v2
    | Extract(h,l,e) ->
	let v = get_expr e in
	let (v,t) = Arithmetic.extract h l v in
	Int (v,t)
    | Concat(le,re) ->
	let vl = get_expr le
	and vr = get_expr re in
	let (v,t) = Arithmetic.concat vl vr in
	Int (v,t)
    | BinOp (op,e1,e2) ->
        let v1 = get_expr e1 
        and v2 = get_expr e2 in
        let (v,t) = Arithmetic.binop op v1 v2 in
        Int (v,t)
    | UnOp (op,e) ->
        let v = get_expr e in
        let (v,t) = Arithmetic.unop op v in
        Int (v,t)
    | Let (var,e1,e2) ->
        let v1 = eval_expr (delta,mu,e1) in
        let delta' = context_copy delta in
        context_update delta' var v1 ;
        let v2 = eval_expr (delta',mu,e2) in
        v2
    | Load (e1,e2,e3,t) ->
        let v1 = eval_expr (delta,mu,e1) 
        and v2 = eval_expr (delta,mu,e2)
        and v3 = eval_expr (delta,mu,e3) in
        let arr = BinOp (PLUS,v1,v2) in
        (match t with
         | Array _ -> (* LOAD_array *)
             failwith "loading array currently unsupported"
         | Reg bits -> (* Load to register *)
             let n = bits/8 in (* FIXME: 1-bit loads? *)
             (* loading the bytes *)
             let rec get_bytes offset acc =
               if offset = n then acc
               else 
                 let mem_index = BinOp (PLUS,arr,Int(biconst offset, Reg 64)) in
                 let index = eval_expr (delta,mu,mem_index) in
                 let byte = lookup_mem mu index in
                 get_bytes (offset+1) (byte::acc)
             in
             (* changing the order according to the endianness *)
             let loaded = 
               let bytes = get_bytes 0 [] in
               if v3 = exp_false then bytes else List.rev bytes
             and byte_size = Int(bi8,Reg 64) in
             (* calculating the loaded value *)
             let value = 
               List.fold_left
                 (fun v n ->
                    let shl = (BinOp(LSHIFT,v,byte_size)) in
                    BinOp(OR,shl,n)
                 ) (Int(bi0,Reg 64)) loaded 
             in
             value
         | _ -> failwith "not a loadable type"
        )
    | Store (e1,e2,e3,e4,t) ->
        let v1 = eval_expr (delta,mu,e1) 
        and v2 = eval_expr (delta,mu,e2) 
        and v3 = eval_expr (delta,mu,e3) in
        let arr = BinOp (PLUS,v1,v2) in
        (match t with
         | Array _ -> (* STORE_array *)
             failwith "storing array currently unsupported"
         | Reg bits -> 
             let v4 = eval_expr (delta,mu,e4) in
             let n = bits/8 in (* FIXME: 1-bit stores? *)
             let lsb = 0xffffL in
             (* Break the value down to bytes *)
             let rec get_bytes offset (v,pos,vals) =
               if offset = n then (v,pos,vals)
               else 
                 let index = BinOp (PLUS,arr,Int(biconst offset, Reg 64)) in
                 let ind = eval_expr (delta,mu,index) in
                 let byte = BinOp (AND,v,Int(big_int_of_int64 lsb, Reg 64)) in
                 let ebyte = eval_expr (delta,mu,byte) in
                 let v' = (BinOp (RSHIFT,v,Int(bi8,Reg 64))) in
                 get_bytes (offset+1) (v',ind::pos,ebyte::vals)
             in
             let _,poss,vals = get_bytes 0 (v3,[],[]) in
             (* Changing the indices based on endianness *)
             let poss = if v4 = exp_false then poss else List.rev poss in
             List.iter2 (fun pos value -> mu_update mu pos value) poss vals ;
             v3
         | _ -> failwith "not a storable type"
        ) 
    | Cast _ 
    | Unknown _ -> failwith "unimplemented"
  in 
  eval expr


let assert_varval = function
  | v when is_true v -> ()
  | v when is_false v -> failwith "Failed assertion"
  | Int _ -> failwith "assert: type error"
  | v -> raise (Symbolic(Exp v))

(* The statement evaluation is practically a transition: *
 * (Delta,Mu,pc,stmt) -> (Delta',Mu',pc',stmt')          *
 * The contexts Sigma, Lambda remain unchanged during    *
 * transitions, but that can be easily changed           *)
let rec step_stmt ?(do_assert=assert_varval) eval_expr {delta=delta;sigma=sigma;lambda=lambda;pc=pc;halt=halt} = 
  let get_label e =
    let v = eval_expr delta e in
    match lab_of_exp v with
    | None -> failwith "not a valid label"
    | Some lab -> label_decode lambda lab
  in
  match inst_fetch sigma pc with
  | Move (v,e,_) ->
      let ev = eval_expr delta e in
      context_update delta v ev ;
      let pc' = succ pc in
      (delta,pc',halt)
  | Halt (_, _) -> 
      (* we don't care about the return value for the time being *)
      (delta,pc,true)
  | Jmp (e,_) -> 
      let pc' = get_label e in
      (delta,pc',halt)
  | CJmp (b,e1,e2,_) ->
      let pc' = match eval_expr delta b with
        | v when is_true v  -> get_label e1
        | v when is_false v -> get_label e2
        | Int _ -> failwith "not a boolean condition or is_true/is_false need redefined"
	| e -> raise(Symbolic(Cond(e,e1,e2)))
      in
      (delta,pc',halt)
  | Assert (e,_) ->
      let v = eval_expr delta e in
      do_assert v;
      (delta, succ pc, halt)
  | Label _
  | Comment _ ->
      (delta, succ pc, halt)
  | _ -> failwith "unimplemented"


let rec eval_stmt eval_expr ({delta=delta; pc=pc; halt=halt} as ctx) = 
  if halt then (delta,pc,halt)
  else
    let (delta',pc',halt) = step_stmt eval_expr ctx in
    eval_stmt eval_expr {ctx with pc=pc'; halt=halt}

(* Symbol meanings:                              *
 * pc: the program counter                       *
 * Sigma: mapping the program counter to a stmt  *
 * Lambda: mapping a label to a program counter  *
 * Delta: mapping a variable to a value          *
 * Mu: mapping a memory index to a value         *)
let program_to_ctx stmts =
  let sigma : (addr, instr) Hashtbl.t = Hashtbl.create 5700
  and initpc = 0
  and delta (*: varval VH.t*) = VH.create 5700
  and lambda : (label_kind, addr) Hashtbl.t = Hashtbl.create 5700 in
  (* Initializing Sigma and Lambda *)
  let lastpc = 
    List.fold_left
       (fun pc s ->
          Hashtbl.add sigma pc s ;
          (match s with
           | Label (lab,_) ->
               Hashtbl.add lambda lab pc
           | _ -> () 
          ) ;
          pc+1
       ) initpc stmts
  in
  Hashtbl.add sigma lastpc (Halt(Unknown("end of program", reg_1),[]));
  {delta=delta; sigma=sigma; lambda=lambda; pc=initpc; halt=false}

(*
let eval_ast_program stmts =
  let ctx = program_to_ctx stmts in
  ignore (eval_stmt eval_expr ctx) ;
  print_values delta ;
  print_mem mu
*)

let eval_ast_program_symb stmts =
  let ctx = program_to_ctx stmts in
  ignore (eval_stmt eval_expr_symb ctx)



let fse post stmts =
  let path_predicate = ref []
  and disjunctions = ref []
  and stack = ref []
  and ctx = ref (program_to_ctx stmts) in
  let do_assert = function
    | v when is_true v -> ()
    (* FIXME: optimize | v when is_false v -> *)
    | v -> list_push path_predicate v
  in
  let halted () = (!ctx).halt
  and step () =
    let  c = !ctx in
    let (delta, pc, halt) = step_stmt ~do_assert eval_expr_symb c in
    (* dprintf "Next pc: %x" pc; *)
    ctx := {c with delta=delta; pc=pc; halt=halt}
  in
  let rec run_some () = (* run until the next exception *)
    match halted() with
    | false ->
	step();
	run_some()
    | true ->
	let p = eval_expr_symb (!ctx).delta post in
	list_push disjunctions (list_join exp_and (p:: !path_predicate));
	match !stack with
	| (delta, pc, pp)::s ->
	    stack := s;
	    pdebug "popped state";
	    path_predicate := pp;
	    ctx := { !ctx with delta=delta; pc=pc; halt=false};
	    run_some()
	| [] -> () (* we're all done *)
  in
  let get_pc ctx e =
    let v = eval_expr_symb ctx.delta e in
    match lab_of_exp v with
    | None -> failwith "not a concrete label in symbolic cjmp"
    | Some lab -> label_decode ctx.lambda lab
  in
  let rec run () =
    try run_some()
    with Symbolic(Cond(e,e1,e2)) ->
      let c = !ctx in
      let d = context_copy c.delta in
      list_push stack (d, get_pc c e1, e :: !path_predicate);
      dprintf "pushed state";
      path_predicate := exp_not e :: !path_predicate;
      ctx := { c with pc = get_pc c e2};
      run()
  in
  run();
  list_join exp_or !disjunctions
      
