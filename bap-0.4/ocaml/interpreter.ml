(* T E S T I N G *)

open Ast
open Template
  
module D = Debug.Make(struct let name = "Interp" and default=`Debug end)
open D

(*
(*   A dummy example using the generic template  *)

module ITypes =
struct 
  type environ = unit
  type state = unit
  type result_exp = unit
  type result_stmt = unit
end

module EvalExpr = 
struct
  let var _ _ _ = ()
  let int _ _ _ = ()
  let lab _ _ _ = ()
  let binop _ _ _ = ()
  let unop _ _ _ = ()
  let cast _ _ _ = ()
  let lett _ _ _ = ()
  let load _ _ _ = ()
  let store _ _ _ = ()
  let unknown _ _ _ = ()
end

module EvalStmt =
struct
  let move _ _ _ = ()
  let halt _ _ _ = ()
  let jmp _ _ _ = ()
  let cjmp _ _ _ = ()
  let assertt _ _ _ = ()
 let comment _ _ _ = ()
  let label _ _ _ = ()
  let special _ _ _ = ()
end

module Test = Abstract(ITypes)
module Inter = Test.Make(EvalExpr)(EvalStmt)

*)

(** A concrete interpreter using the generic template **)

open Int64
open Type
open Unix

module VH = Var.VarHash
module AddrMap = Map.Make(Int64)

type addr = int64
type expr = Scalar of (int64 * typ) | Mem of (expr AddrMap.t)

module MyTypes =
struct 
  type result_exp = expr
  type result_stmt = unit
  type state = {
    delta       : expr VH.t;
    filesystem  : (int64, file_descr) Hashtbl.t;
    mutable pc  : addr
  }
  type environ = {
    sigma  : (addr, stmt) Hashtbl.t;
    lambda : (label, addr) Hashtbl.t;
    expr   : Ast.exp -> environ -> state -> result_exp
  }
end

exception Unknown_target
exception Unknown_label
exception Terminated
exception Assertion_failed
exception Unknown_expr

(***********************************************************)
(************************ Helpers **************************)
(***********************************************************)
let def = Scalar (0L, reg_1)
let def_mem = Mem AddrMap.empty
let def_mem_val = Scalar (0L, reg_8)
let def_var = Scalar (0xbfffffffL, reg_32)

let unwrap_scalar = function
  | Scalar s -> s
  | Mem _ -> failwith "Unwrapping memory!!"

let unwrap_mem = function
  | Mem m -> m
  | Scalar _ -> failwith "Unwrapping scalar!!"

let is_mem var = match Var.typ var with
  | TMem _ -> true
  | _ -> false

let is_addr l = String.sub l 0 3 = "pc_" 

let lab_to_addr l = 
  Addr (Int64.of_string (String.sub l 3 (String.length l - 3)))

let eval_expr e env st = 
(*  pdebug (Pp.ast_exp_to_string e) ;*)
  unwrap_scalar (env.MyTypes.expr e env st)

(***********************************************************)
(***************** Variable Manipulation *******************)
(***********************************************************)

let get_var delta v = 
  try VH.find delta v
  with Not_found ->
    if is_mem v then def_mem 
    else def_var
      (*failwith ("Undefined variable " ^ (Var.name v))*)

let var_by_name delta name =
  let len = String.length name in
  let var = VH.fold (fun k _ found -> 
		       let kname = String.sub (Var.name k) 0 len in
		       if kname = name then Some k
		       else found
		    ) delta None
  in
    match var with
      | None -> failwith ("not found " ^ name)
      | Some v -> v
	  
let set_var = VH.replace
let ovw_var = VH.add
let del_var = VH.remove

(************************* Labels *************************)

let get_lab lambda addr = 
  try Hashtbl.find lambda addr
  with Not_found -> raise Unknown_label

let goto e env st = match e with
  | Int(n,_) -> 
      if Hashtbl.mem env.MyTypes.sigma n then st.MyTypes.pc <- n
      else st.MyTypes.pc <- get_lab env.MyTypes.lambda (Addr n)
  | _ -> (pdebug "goto unknown" ; raise Unknown_target)
(*  pdebug (Pp.ast_exp_to_string e) ;
  try st.MyTypes.pc <- match lab_of_exp e with
    | None -> raise Not_found
    | Some l -> get_lab env.MyTypes.lambda l
  with Not_found -> raise Unknown_label
*)

let inc_pc st = st.MyTypes.pc <- add st.MyTypes.pc 1L

(************************ Memories ************************)

let load_mem m a = 
  try AddrMap.find a m
  with Not_found -> 
(    dprintf "mem[%Lx] = ?\n" a ;
     (*ignore (Pervasives.input_char Pervasives.stdin );*)
    def_mem_val )

let store_mem m a v = AddrMap.add a v m

let get_string m a = 
  let rec get_byte acc a = function 
    | Scalar(0L,_) -> acc
    | Scalar(n,_) -> 
	let c = Char.escaped (char_of_int (Int64.to_int n)) in
	let next = add a one in
	get_byte (acc^c) next (load_mem m next)
    | _ -> failwith "found non-scalar in memory"
  in
    get_byte "" a (load_mem m a)

(***********************************************************)
(************************ Specials *************************)
(***********************************************************)

module Specials =
struct

  let fdcount = ref 4L

  let interrupt st = 
    let get_reg name = 
      let var =  var_by_name st.MyTypes.delta name in
	match get_var st.MyTypes.delta var with
	  | Scalar(num,_) -> num
	  | _ -> failwith "not a number"
    in
    let get_mem () = 
      let mem = var_by_name st.MyTypes.delta "mem" in
	match get_var st.MyTypes.delta mem with
	  | Mem m -> m
	  | _ -> failwith "the impossible happened"
    in
    let eax = get_reg "R_EAX" in
      match eax with
	| 1L -> (* EXIT *)
	    (
	      let ebx = get_reg "R_EBX" in
		match ebx with
		  | 0L -> raise Terminated (* exit *)
		  | _ -> failwith "unsupported"
	    )
	      (* TODO: Update the registers after the return of the syscall *)
	| 3L -> (* READ *)
	    (
	      (* File Descriptor *)
	      let ebx = get_reg "R_EBX" in
	      let fd = 
		try Hashtbl.find st.MyTypes.filesystem ebx
		with Not_found -> failwith "unknown file descriptor"
	      in
	      (* Buffer *)
	      let ecx = get_reg "R_ECX" in
		(* Count *)
	      let edx = get_reg "R_EDX" in
	      let mem_var = var_by_name st.MyTypes.delta "mem" in
	      let read_byte () = 
		let s = String.create 1 in
		  match read fd s 0 1 with
		    | 0 -> raise End_of_file
		    | _ -> s.[0]
	      in
	      let rec read_bytes addr = function
		| 0L -> ecx
		| n ->
		    let byte = 
		      Scalar (Int64.of_int (int_of_char (read_byte ())), reg_8) 
		    in
		    let mem = get_mem () in
		    set_var st.MyTypes.delta mem_var 
		      (Mem (store_mem mem addr byte)) ;
		    read_bytes (add addr one) (sub n one)
	      in
	      let byte_num = try read_bytes ecx edx with End_of_file -> 0L in
	      let eax = var_by_name st.MyTypes.delta "R_EAX" in
		set_var st.MyTypes.delta eax (Scalar (byte_num, reg_32))
	    )	    
	| 4L -> (* WRITE *)
	    (
	      (* File Descriptor *)
	      let ebx = get_reg "R_EBX" in
	      let fs = 
		try
		  let fd = Hashtbl.find st.MyTypes.filesystem ebx in
		    out_channel_of_descr fd
		with Not_found -> failwith "trying to write to a closed fd"
	      in
		(* Buffer *)
	      let ecx = get_reg "R_ECX" in
		(* Count *)
	      let edx = get_reg "R_EDX" in
	      let mem = get_mem () in
	      let print_byte b = output_char fs (char_of_int (Int64.to_int b)) in
	      let rec write_bytes addr = function
		| 0L -> edx
		| n ->
		    let byte = fst (unwrap_scalar (load_mem mem addr)) in
		      print_byte byte ;
		    write_bytes (add addr one) (sub n one)
	      in
		let byte_num = write_bytes ecx edx in
		flush fs ;
		  let eax = var_by_name st.MyTypes.delta "R_EAX" in
		    set_var st.MyTypes.delta eax (Scalar (byte_num, reg_32))
	    )
	| 5L -> (* OPEN *)
	    (* the path string *)
	    let ebx = get_reg "R_EBX" in
	      (* the flags *)
	    let ecx = get_reg "R_ECX" in
	    let mem = get_mem () in
	    let file = get_string mem ebx in
	    let mode = match Int64.logand 3L ecx with (* keep the lowest bits *)
	      | 0L -> O_RDONLY
	      | 1L -> O_WRONLY
	      | 2L -> O_RDWR
	      | _ -> failwith ("unsupported mode " ^ (Int64.to_string ecx))
	    in
	    let fd = Unix.openfile file [mode] 0o755 in
	      Hashtbl.add st.MyTypes.filesystem !fdcount fd ;
	      let eax = var_by_name st.MyTypes.delta "R_EAX" in
		set_var st.MyTypes.delta eax (Scalar (!fdcount, reg_32)) ;
		fdcount := add !fdcount one ;
	      pdebug ("opening " ^ file)
	| 122L -> (* UNAME *)
	    let eax = var_by_name st.MyTypes.delta "R_EAX" in
	      set_var st.MyTypes.delta eax (Scalar (0L, reg_32))
	| 146L -> (* WRITEV *)
	    (* file descriptor *)
	    (*let ebx = get_reg "R_EBX" in
	    let fs = 
	      try 
		let fd = Hashtbl.find st.MyTypes.filesystem ebx in
		  out_channel_of_descr fd
	      with Not_found -> failwith "trying to writev to a closed fd"
	    in
	      (* the iovecs *)
	    let ecx = get_reg "R_ECX" in
	      (* the iovec count *)
	    let edx = get_reg "R_EDX" in
	    let print_byte b = output_char fs (char_of_int (Int64.to_int b)) in
	    let rec write_buf num addr = function
		| 0L -> num
		| n ->
		    print_byte n ;
		    let addr' = add addr one in
		    let next = fst (unwrap_scalar (load_mem mem addr')) in
		      write_buf (add num one) addr' next
	    in
	    let rec write_bufs nbytes vec = function 
	      | 0L -> nbytes
	      | n -> 
		  let bytes = write_buf zero addr (load_mem mem addr) in
		    write_bufs (nbytes + bytes) (sub n one)
	    in
	      write_bufs *)()
	| _ -> failwith ("unsupported eax = " ^ (Int64.to_string eax))

  let handle s st = match String.sub s 0 3 with
    | "cal" | "ret" -> ()
    | "int" -> interrupt st
    | _ -> failwith "fail"

end

(***********************************************************)

(* Handling expressions *)

module RunExpr = 
struct
  let var v _env st = 
    get_var st.MyTypes.delta v
  let int (i,t) _env _st = Scalar(i,t)
  let lab l env _st = 
    let lab = if is_addr l then lab_to_addr l else Name l in
      Scalar(get_lab env.MyTypes.lambda lab, reg_64)
  let ite (cond,e1,e2) env st =
    let ce = eval_expr cond env st
    and e1 = eval_expr e1 env st
    and e2 = eval_expr e2 env st in
    Scalar(if Int ce = exp_true then e1 else e2)
  let extract (h,l,e) env st =
    let e = eval_expr e env st in
    Scalar(Arithmetic.extract h l e)
  let concat (le,re) env st =
    let le = eval_expr le env st in
    let re = eval_expr re env st in
    Scalar(Arithmetic.concat le re)
  let binop (op,e1,e2) env st = 
    let e1 = eval_expr e1 env st
    and e2 = eval_expr e2 env st in
      Scalar(Arithmetic.binop op e1 e2)
  let unop (op,e) env st = 
    let e = eval_expr e env st in
      Scalar(Arithmetic.unop op e)
  let cast (ct,t,e) env st = 
    let e = eval_expr e env st in
      Scalar(Arithmetic.cast ct e t)
  let lett (v,e1,e2) env st = 
    let e1 = env.MyTypes.expr e1 env st in
      ovw_var st.MyTypes.delta v e1;
      let e2 = env.MyTypes.expr e2 env st in
	del_var st.MyTypes.delta v;
	e2
  let load (m,a,e,t) env st = 
    let m' = unwrap_mem (env.MyTypes.expr m env st)
    and a = eval_expr a env st 
    and e = eval_expr e env st in
      match t with
	| Reg 8 -> load_mem m' (fst a)
	| Reg _ ->
	    let expr = Memory2array.split_loads m (Int a) t (Int e) in
	      env.MyTypes.expr expr env st
        | _ -> failwith "unsupported memory load type"

  let store (m,a,v,e,t) env st =
    let m' = unwrap_mem (env.MyTypes.expr m env st)
    and a = eval_expr a env st
    and e = eval_expr e env st 
    and v = eval_expr v env st in
    let stored = match t with
      | Reg 8 -> store_mem m' (fst a) (Scalar v)
      | Reg _ ->
	  let expr = 
	    Memory2array.split_writes m (Int a) t (Int e) (Int v)
	  in
	    unwrap_mem (env.MyTypes.expr expr env st)
      | _ -> failwith "unsupported memory store type"
    in
      Mem stored
	    
  let unknown _ _ _ = raise Unknown_expr
end

(* Handling statements *)

module RunStmt =
struct
  let move (v,e,_) env st = 
    let e = env.MyTypes.expr e env st in
      set_var st.MyTypes.delta v e;
      inc_pc st
  let halt (e,_) env st = 
    let _e = env.MyTypes.expr e env st in
      raise Terminated
  let jmp (e,_) env st = 
    let e = eval_expr e env st in
      goto (Int e) env st
  let cjmp (b,e1,e2,_) env st = 
    let b = eval_expr b env st 
    and e1 = eval_expr e1 env st 
    and e2 = eval_expr e2 env st in
      if Int b = exp_true then goto (Int e1) env st
      else goto (Int e2) env st
  let assertt (e,_) env st = 
    let e = eval_expr e env st in
      if Int e = exp_false then raise Assertion_failed
      else inc_pc st
  let comment _ _ st = inc_pc st
  let label _ _ st = inc_pc st
  let special (s,_) _ st = Specials.handle s st ;
    inc_pc st
end

(* The actual interpreter *)

module MyTemplate = Abstract(MyTypes)
module Interpret = MyTemplate.Make(RunExpr)(RunStmt)

let init_pc = 0L

(* let's convert the program in a more convenient form *)
let create_environ stmts = 
  let sigma = Hashtbl.create 5700 in
  let lambda = Hashtbl.create 5700 in
    ignore 
      (List.fold_left
         (fun pc s ->
	    Hashtbl.add sigma pc s ;
	    (match s with
	       | Label (l,_) -> Hashtbl.add lambda l pc
	       | _ -> () 
	    ) ;
	    Int64.succ pc
         ) init_pc stmts 
      ) ;
    {
      MyTypes.sigma = sigma; 
      MyTypes.lambda = lambda;
      MyTypes.expr = Interpret.expr;
    }
      
let create_state () =
  let filesystem = Hashtbl.create 10 in
    Hashtbl.add filesystem 0L stdin ;
    Hashtbl.add filesystem 1L stdout ;
  {
    MyTypes.delta = VH.create 5700 ; 
    MyTypes.filesystem = filesystem ;
    MyTypes.pc = init_pc
  }

let get_stmt {MyTypes.sigma = sigma} {MyTypes.pc = pc} =
  try Hashtbl.find sigma pc
  with Not_found -> raise Unknown_target

let execute ast_prog = 
  let env = create_environ ast_prog in
  let st = create_state () in
    (try 
       while true do 
	 (*pdebug (Pp.ast_stmt_to_string (get_stmt env st)) ;*)
	 Interpret.stmt (get_stmt env st) env st
       done ;
     with 
       | Unknown_target -> failwith "Jumping out of Dom(Sigma)"
       | Terminated -> 
	   Printf.printf "Successful termination\n" (*;
	   VH.iter 
	     (fun var value -> 
		if not (is_mem var) then
		  Printf.printf "%s = %Lx\n" 
		    (Var.name var) 
		    (fst (unwrap_scalar value))
	     ) 
	     st.MyTypes.delta*)
    ) ;
    ast_prog
    
