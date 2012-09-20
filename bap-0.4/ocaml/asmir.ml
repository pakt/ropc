(** High level interface to libasmir.
    
    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

open Asmirconsts
open Ast
open Big_int
open BatListFull
open Libasmir
open Libbfd
open Type
open Util

module BArray = Bigarray.Array1

exception Disassembly_error;;
exception Memory_error;;

let always_vex = ref false;;

type arch = Libbfd.bfd_architecture
type asmprogram = {asmp : Libasmir.asm_program_t;
		   arch : arch;
		   secs : section_ptr list;
		   get : int64 -> char }


let arch_i386 = Bfd_arch_i386
let arch_arm  = Bfd_arch_arm
(*more to come later when we support them*)

let trace_blocksize = ref 100000L

module D = Debug.Make(struct let name = "ASMIR" and default=`Debug end)
open D

module Status = Util.StatusPrinter

(* more verbose debugging *)
module DV = Debug.Make(struct let name = "AsmirV" and default=`NoDebug end)
(* module DCheck = Debug.Make(struct let name = "AsmirCheck" and default=`NoDebug end) *)

(* Debug output for testing*)
module DTest = Debug.Make(struct let name = "AsmirTest" and default=`NoDebug end)

(** Translate a unop *)
let tr_unop = function
  | Libasmir.NEG -> NEG
  | Libasmir.NOT -> NOT

(** Translate a type *)
let tr_regtype = function
  | Libasmir.REG_1   -> reg_1 
  | Libasmir.REG_8   -> reg_8 
  | Libasmir.REG_16  -> reg_16
  | Libasmir.REG_32  -> reg_32
  | Libasmir.REG_64  -> reg_64

(* maps a string variable to the var we are using for it *)
type varctx = (string,Var.t) Hashtbl.t

(** [gamma_create mem decls] creates a new varctx for use during translation. 
    [mem] is the var that should be used for memory references, and [decls]
    should be a list of variables already in scope.
*)
let gamma_create mem decls : varctx =
  let h = Hashtbl.create 57 in
  List.iter (fun (Var.V(_,nm,_) as var) -> Hashtbl.add h nm var) decls;
  Hashtbl.add h "$mem" mem;
  Hashtbl.add h "mem" mem;
  h

let gamma_lookup (g:varctx) s =
  try Hashtbl.find g s
  with Not_found ->
    failwith("Disassembled code had undeclared variable '"^s^"'. Something is broken.")

let gamma_extend = Hashtbl.add


let gamma_unextend = Hashtbl.remove

(* Translate a string label into a name or address label as approriate *)
let tr_label s =
  Name s (* FIXME: treating them all as names for now *)

(** Translate an expression *)
let rec tr_exp g e =
  match Libasmir.exp_type e with
    | BINOP ->
	tr_binop g (Libasmir.binop_type e) (Libasmir.binop_lhs e) (Libasmir.binop_rhs e)
    | UNOP ->
        UnOp(tr_unop(Libasmir.unop_type e),
	     tr_exp g (Libasmir.unop_subexp e) )
    | CONSTANT ->
        Int(big_int_of_int64 (Libasmir.constant_val e), tr_regtype (constant_regtype e))
    | MEM ->
	let mem = gamma_lookup g "$mem" in
	let wtyp = tr_regtype (mem_regtype e) in 
	Load(Var mem, tr_exp g (mem_addr e), little_endian, wtyp)
    | TEMP ->
	let nm = temp_name e  in
	let Var.V(_,_,t) as var = gamma_lookup g nm in
	(*let t' = tr_regtype(temp_regtype e) in
	if t <> t'
	then failwith("Disassembly produced incorrect type "^type_to_string t'^" for"^var_to_string var)
	else*) Var var
    | CAST ->
        let sube = tr_exp g (cast_subexp e) in
        let newt = tr_regtype(cast_width e) in
	(match cast_casttype e with
	 | Libasmir.CAST_UNSIGNED -> Cast(CAST_UNSIGNED, newt, sube)
	 | Libasmir.CAST_SIGNED   -> Cast(CAST_SIGNED, newt, sube)
	 | Libasmir.CAST_HIGH	   -> Cast(CAST_HIGH, newt, sube)
	 | Libasmir.CAST_LOW	   -> Cast(CAST_LOW, newt, sube)
	 | Libasmir.CAST_FLOAT
	 | Libasmir.CAST_INTEGER
	 | Libasmir.CAST_RFLOAT
	 | Libasmir.CAST_RINTEGER ->
	     (pwarn "Warning: Ignoring deprecated cast type\n"; sube)
	)
    | NAME ->
	(match tr_label (name_string e) with
	 | Name n -> Lab n
	 | Addr i -> Int(big_int_of_int64 i, reg_64)
	)
    | UNKNOWN ->
        Unknown(unknown_str e, tr_regtype(unknown_regtype e))
    | LET ->
	failwith "Let expressions from C++ no longer supported"
    | EXTENSION ->
	failwith "Extension stmt types are unsupported."
    | _ ->
	failwith "Unexpected stmt type"


(** Translate a binop *)
and tr_binop g b lhs rhs =
  let (lhs,rhs) = (tr_exp g lhs, tr_exp g rhs) in
  match b with
    | Libasmir.PLUS     -> BinOp(PLUS    , lhs, rhs)
    | Libasmir.MINUS	-> BinOp(MINUS   , lhs, rhs)
    | Libasmir.TIMES	-> BinOp(TIMES   , lhs, rhs)
    | Libasmir.DIVIDE	-> BinOp(DIVIDE  , lhs, rhs)
    | Libasmir.SDIVIDE	-> BinOp(SDIVIDE  , lhs, rhs)
    | Libasmir.MOD	-> BinOp(MOD     , lhs, rhs)
    | Libasmir.SMOD	-> BinOp(SMOD     , lhs, rhs)
    | Libasmir.LSHIFT	-> BinOp(LSHIFT  , lhs, rhs)
    | Libasmir.RSHIFT	-> BinOp(RSHIFT  , lhs, rhs)
    | Libasmir.ARSHIFT	-> BinOp(ARSHIFT , lhs, rhs)
    | Libasmir.LROTATE
    | Libasmir.RROTATE	-> failwith "rotate is deprecated"
    | Libasmir.LOGICAND -> BinOp(AND  , lhs, rhs) (* operands should be bool *)
    | Libasmir.LOGICOR	-> BinOp(OR   , lhs, rhs) (* operands should be bool *)
    | Libasmir.BITAND	-> BinOp(AND  , lhs, rhs)
    | Libasmir.BITOR	-> BinOp(OR   , lhs, rhs)
    | Libasmir.XOR	-> BinOp(XOR     , lhs, rhs)
    | Libasmir.EQ	-> BinOp(EQ      , lhs, rhs)
    | Libasmir.NEQ	-> BinOp(NEQ     , lhs, rhs)
        (* FIXME: Assuming all comparisons are unsigned.
           This should be valid for IR generated via VEX. 
        *)
    | Libasmir.LT	-> BinOp(LT, lhs, rhs)
    | Libasmir.LE       -> BinOp(LE, lhs, rhs)
        (* We don't have GT or GTE, so implement using LT and LTE *)
    | Libasmir.GT	-> BinOp(LE, rhs, lhs) (* (x > y) <-> (y <= x) *)
    | Libasmir.GE	-> BinOp(LT, rhs, lhs) (* (x >= y) <-> (y < x) *)


(** Translate a vardecl, and adds the variable to the context
    
    @return vardecl and a function to restore the context
*)
let tr_vardecl (g:varctx) s =
  assert(Libasmir.stmt_type s = VARDECL);
  let nm = Libasmir.vardecl_name s in 
  let var = Var.newvar nm (tr_regtype(Libasmir.vardecl_type s)) in
  gamma_extend g nm var;
  (var, fun () -> gamma_unextend g nm)
    
(** Translate a list of vardecls, adding them to the context.
    @return vardecls and a function to restore the context *)
let tr_vardecls g ss =
  let decls,unextends = List.split(List.map (tr_vardecl g) ss) in
  (decls, fun x -> List.iter (fun f -> f x) unextends)

let cval_type_to_typ = function
 | NONE -> 
   prerr_endline "concrete expression with no type in lifted trace" ;
   reg_32        
 | BOOL -> reg_1
 | CHR -> reg_8
 | INT_16 -> reg_16
 | INT_32 -> reg_32
 | INT_64 -> reg_64
 | INT_128 -> Reg 128

let get_cval_usage = function
  | 0x00 | 0x01 -> RD
  | 0x10 -> WR
  | 0x11 -> RW
  | _ -> 
      prerr_endline "expression with no usage info" ;
      RD
      

(* TODO: needs to be refined for bytes *)
let int_to_taint n = Taint n

let big_int_of_big_val v t =
  let big_int_of_big_val_help v =
    let n = (cval_value_size v) - 1 in
    Util.foldn
      (fun acc index ->
         (* On x86, the data will be stored in litle endian form, so the
	    last index has the most significant data.  We want to access
	    this first, since we shift left as we go. *)
         let revindex = n - index in
         (* We need to convert the int64 we need to two's complement form *)
         let tempv = Arithmetic.to_big_int (big_int_of_int64 (cval_value_part v revindex), reg_64) in
         let shiftacc = shift_left_big_int acc 64 (* sizeof(int64) *) in
         (* dprintf "Hmmm.... %s %s" (string_of_big_int tempv) (string_of_big_int shiftacc); *)
         add_big_int tempv shiftacc)
      zero_big_int
      n
  in
  (* Cast off useless bits *)
  Arithmetic.to_big_int (big_int_of_big_val_help v, t)


let tr_context_tup cval =
  let t = cval_type_to_typ (Libasmir.cval_type cval) in
  Context {name=Libasmir.cval_name cval;
           mem=Libasmir.cval_mem cval;
           t=t;
           index=Libasmir.cval_ind cval;
           value=big_int_of_big_val (Libasmir.cval_value cval) t;
	   usage=get_cval_usage(Libasmir.cval_usage cval);
           taint=int_to_taint (Libasmir.cval_taint cval)}

(* deprecated *)
let tr_attributes s =
  let attr_vec = Libasmir.stmt_attributes s in
  let size = Libasmir.conc_map_size attr_vec in
  let cvals =
    if size = 0 then [] 
    else
      foldn 
	(fun i n -> 
	   (tr_context_tup (Libasmir.get_cval attr_vec n))::i
	) [] (size-1) in
  let tid = Libasmir.trace_tid attr_vec in
  let tidattr = ThreadId tid in
  if tid = -1 then cvals else
  tidattr :: cvals

(** Given a trace frame, return the list of attrs. *)
let tr_frame_attrs f =
  let attr_vec = Libasmir.asmir_frame_get_operands f in
  let size = Libasmir.asmir_frame_operands_length attr_vec in
  (* Add concrete operands *)
  let attrs =
    if size = 0 || size = -1 then [] 
    else
      let cattrs = foldn 
	(fun i n -> 
	   (tr_context_tup (Libasmir.asmir_frame_get_operand attr_vec n))::i
	) [] (size-1) 
      in
      let () = Libasmir.asmir_frame_destroy_operands attr_vec 
      in
      cattrs
  in
  (* Add ThreadId *)
  let attrs = match Libasmir.asmir_frame_tid f with
    | -1 -> attrs
    | n -> (Type.ThreadId n)::attrs
  in
  attrs

(** Translate a statement *)
let rec tr_stmt g s =
  match Libasmir.stmt_type s with
      JMP ->
	Jmp(tr_exp g (Libasmir.jmp_target s), [])
    | CJMP ->
	CJmp(tr_exp g (Libasmir.cjmp_cond s),
	    tr_exp g (Libasmir.cjmp_ttarget s),
	    tr_exp g (Libasmir.cjmp_ftarget s),
	    [] )
    | SPECIAL ->
	Special(Libasmir.special_string s, [])
    | MOVE ->
	let e = tr_exp g (move_rhs s) in
	(match tr_exp g (move_lhs s) with
	 | Var v ->
	     Move(v, e, [])
	 | Load(Var var as v, idx, endi, w) ->
	     Move(var, Store(v, idx, e, endi, w), [])
	 | _ -> 
	     failwith "Inproper lvalue in move"
	)
    | COMMENT ->
	Comment(Libasmir.comment_string s, [])
    | LABEL ->
	Label(tr_label (Libasmir.label_string s),
          tr_attributes s)
    | ASSERT ->
	Assert(tr_exp g (Libasmir.assert_cond s), [])
    | VARDECL
    | EXPSTMT
    | CALL
    | RETURN
    | FUNCTION ->
	failwith "Unsupported statement type"


(* convert certain specials into attributes *)
let rec handle_specials = 
  let reta = StrAttr "ret"
  and calla = StrAttr "call" in
  function
    | Jmp(t,a) :: Special("ret", _) :: stmts ->
      Jmp(t, reta::a) :: stmts
    | Jmp(t,a) :: Special("call", _) :: stmts ->
      Jmp(t, calla::a) :: stmts
    | x::xs -> x :: handle_specials xs
    | [] -> []
      

(** Translate a whole bap_block_t (as returned by
    Libasmir.asmir_bap_blocks_get) into a list of statements *)
let tr_bap_block_aux g b =
  let size = Libasmir.asmir_bap_block_size b - 1 in
  if Libasmir.asmir_bap_block_error b then
    failwith "Block was not lifted correctly by vine";
  assert (size+1 > 0);
  let addr = Libasmir.asmir_bap_block_address b in
  let (decs,stmts) =
    foldn (fun (ds,ss) n -> let s = asmir_bap_block_get b n in
	     match Libasmir.stmt_type s with
		 VARDECL -> (s::ds,ss)
	       | _ -> (ds,s::ss) )
      ([],[]) size
  in
  let decls, unextend = tr_vardecls g decs in
  let stmts = List.map (tr_stmt g) stmts in
  let stmts = handle_specials stmts in
  (stmts, addr, unextend)

let tr_bap_block_t g asmp b = 
  let stmts, addr, unextend = tr_bap_block_aux g b in
  let asm = Libasmir.asmir_string_of_insn asmp addr in
  let stmts = Label(Addr addr, [Asm asm])::stmts in 
  unextend();
  stmts

let tr_bap_block_t_trace_asm g b =
  let stmts, addr, unextend = tr_bap_block_aux g b in
  let asm = Libasmir.asm_string_from_block b in
  let stmts = Label(Addr addr, [Asm asm])::stmts in
  unextend();
  stmts

let tr_bap_block_t_no_asm g b =
  let stmts, addr, unextend = tr_bap_block_aux g b in
  let stmts = Label(Addr addr, [])::stmts in
  unextend();
  stmts

(** Translate a bap_blocks_t (as returned by
    Libasmir.asmir_asmprogram_to_bap) into a list of statements *)
let tr_bap_blocks_t g asmp bs = 
  let size = Libasmir.asmir_bap_blocks_size bs -1 in
    foldn (fun i n -> tr_bap_block_t g asmp (asmir_bap_blocks_get bs n)@i) [] size

let tr_bap_blocks_t_trace_asm g bs = 
  let size = Libasmir.asmir_bap_blocks_size bs -1 in
    foldn (fun i n -> tr_bap_block_t_trace_asm g (asmir_bap_blocks_get bs n)@i) [] size

let tr_bap_blocks_t_no_asm g bs = 
  let size = Libasmir.asmir_bap_blocks_size bs -1 in
    foldn (fun i n -> tr_bap_block_t_no_asm g (asmir_bap_blocks_get bs n)@i) [] size

let x86_regs = Disasm_i386.regs
let x86_mem = Disasm_i386.mem
(* let x86_mem_external = Ast.Var (x86_mem) *)

let arm_regs =
  List.map (fun n -> Var.newvar n reg_32)
    [ "R0";     
      "R1";     
      "R2";     
      "R3";     
      "R4";     
      "R5";     
      "R6";     
      "R7";     
      "R8";     
      "R9";     
      "R10";    
      "R11";    
      "R12";    
      "R13";    
      "R14";    
      "R15";    
      "CC";
      "CC_OP";	 
      "CC_DEP1";
      "CC_DEP2";
    ]

let all_regs = x86_mem :: x86_regs @ arm_regs

let decls_for_arch = function
  | Bfd_arch_i386 -> x86_mem::x86_regs
  | Bfd_arch_arm  -> x86_mem::arm_regs
  | _ -> failwith "decls_for_arch: unsupported arch"

let gamma_for_arch = function
  | Bfd_arch_i386 -> gamma_create x86_mem x86_regs
  | Bfd_arch_arm  -> gamma_create x86_mem arm_regs
  | _ -> failwith "gamma_for_arch: unsupported arch"


let get_asmprogram_arch {arch=arch}= arch

let fold_memory_data f md acc =
  let size = Libasmir.memory_data_size md - 1 in
    foldn (fun a n ->
            let mcd = Libasmir.memory_data_get md n in
              f 
              (Libasmir.memory_cell_data_address mcd)
              (Libasmir.memory_cell_data_value mcd) 
              a)
      acc size

(* FIXME: use bfd_get_section_contents instead of this crazy memory_data thing *)
let get_rodata_assignments ?(prepend_to=[]) mem {asmp=prog} =
  let rodata = Libasmir.get_rodata prog in
  fold_memory_data
    (fun a v acc -> 
        let m_addr = Int(big_int_of_int64 a, Reg 32) in
        let m_val = Int(big_int_of_int v, Reg 8) in
        Move(mem, Store(Var mem, m_addr, m_val, little_endian, Reg 8), [InitRO]) :: acc)
    rodata prepend_to

let get_all_sections p =
  let arr,err = Libasmir.asmir_get_all_sections p in
  if err <= 0 then failwith "get_all_sections";
  arr

let get_all_asections p =
  get_all_sections p.asmp

let bfd_section_size = Libbfd.bfd_section_get_size
let bfd_section_vma = Libbfd.bfd_section_get_vma
let bfd_section_name = Libbfd.bfd_section_get_name

let get_section_start = bfd_section_vma
let get_section_end s = 
    let start = bfd_section_vma s in
    let size = bfd_section_size s in
    let (+) = Int64.add in 
    start+size

(** Is section s loaded? *)
let is_load s =
  let flags = bfd_section_get_flags s in
  Int64.logand Libbfd.sEC_LOAD flags <> 0L

(** Is section s code? *)
let is_code s =
  let flags = bfd_section_get_flags s in
  Int64.logand flags Libbfd.sEC_CODE <> 0L

let is_rw_data s = 
  let flags = bfd_section_get_flags s in
  Int64.logand flags Libbfd.sEC_DATA <> 0L && (Int64.logand flags Libbfd.sEC_READONLY = 0L)

let is_bss s = 
  let flags = bfd_section_get_flags s in
  flags = Libbfd.sEC_ALLOC

let section_contents prog secs =
  let bfd = Libasmir.asmir_get_bfd prog in
  let sc l s =
    let size = bfd_section_size s and vma = bfd_section_vma s
    and flags = bfd_section_get_flags s
    and name = bfd_section_name s in
    dprintf "Found section %s at %Lx with size %Ld. flags=%Lx" name vma size flags;
    if is_load s then
    (* if Int64.logand Libbfd.sEC_LOAD flags <> 0L then *)
      let (ok, a) = Libbfd.bfd_get_section_contents bfd s 0L size in
      if ok <> 0 then (vma, a)::l else (dprintf "failed."; l)
    else l
  in
  let bits = List.fold_left sc [] secs in
  let get a =
    (* let open Int64 in *)
    let (-) = Int64.sub in
    let rec f a = function [] -> raise Memory_error
      | (s,arr)::_ when a - s >= 0L && a - s < Int64.of_int(BArray.dim arr)  ->
	  arr.{Int64.to_int(a-s)}
      | _::b -> f a b
    in
    f a bits
  in
  get


(** Open a binary file for translation *)
let open_program filename =
  let prog = Libasmir.asmir_open_file filename in
    (* tell the GC how to free resources associated with prog *)
  Gc.finalise Libasmir.asmir_close prog;
  let secs = Array.to_list (get_all_sections prog)  in
  let get = section_contents prog secs in
  {asmp=prog; arch=Libasmir.asmir_get_asmp_arch prog; secs=secs; get=get}


let get_asm = function
  | Label(_,[Asm s])::_ -> s
  | _ -> ""

(* let check_equivalence a (ir1, next1) (ir2, next2) = *)
(*   assert(next1 = next2); *)
(*   try *)
(*     let q = Var(Var.newvar "q" reg_1) in *)
(*     let to_wp p =  *)
(*       let p = Memory2array.coerce_prog p in *)
(*       Wp.wp (Gcl.of_ast p) q *)
(*     in *)
(*     let wp1 = to_wp ir1 *)
(*     and wp2 = to_wp ir2 in *)
(*     let e = BinOp(EQ, wp1, wp2) in *)
(*     match Smtexec.CVC3.check_exp_validity e with *)
(*     | Smtexec.Valid -> () *)
(*     | Smtexec.Invalid -> wprintf "formulas for %Lx (%s aka %s) not equivalent" a (get_asm ir1) (get_asm ir2) *)
(*     | Smtexec.SmtError -> failwith "SmtError" *)
(*     | Smtexec.Timeout -> failwith "Timeout" *)
(*   with Failure s *)
(*   | Invalid_argument s -> *)
(*     (match get_asm ir1 with (\* Don't warn for known instructions *\) *)
(*     | "ret" | "hlt"-> () *)
(*     | _ -> wprintf "Could not check equivalence for %Lx: %s" a s *)
(*     ) *)
(*   | Not_found -> *)
(*     wprintf "Could not check equivalence for %Lx: Not_found" a *)


(** Translate only one address of a  Libasmir.asm_program_t to Vine *)
let asm_addr_to_bap {asmp=prog; arch=arch; get=get} addr =
  let fallback() =
    let g = gamma_for_arch arch in
    let (block, next) = Libasmir.asmir_addr_to_bap prog addr in
    if Libasmir.asmir_bap_block_error block then
      (* We are unable to lift this address. Decode errors are
         converted to a Special("VEX Decode Error"), so this is a
         non-Decode error.

         Unfortunately, we get no idea of the instruction length when
         this happens, so we'll optimistically increase by one.  *)
      let asm = Libasmir.asmir_string_of_insn prog addr in
      (Disasm_i386.ToIR.add_labels ~asm addr (Special("VEX General Error", [])::[]), Int64.succ addr)
    else
      (* Success: vine gave us a block to translate *)
      let ir = tr_bap_block_t g prog block in
      destroy_bap_block block;
      (ir, next)
  in
  if (!always_vex) then fallback() 
  else (
	try 
      let (ir,na) as v = 
		(try (Disasm.disasm_instr arch get addr)
		 with Failure s -> 
		   DTest.dprintf "BAP unknown disasm_instr %Lx: %s" addr s;
		   DV.dprintf "disasm_instr %Lx: %s" addr s; raise Disasm.Unimplemented
		)
      in
      DV.dprintf "Disassembled %Lx directly" addr;
      (* if DCheck.debug then check_equivalence addr v (fallback()); *)
	  (* If we don't have a string disassembly, use binutils disassembler *)
      (match ir with
      | Label(l, [])::rest ->
		(Label(l, [Asm(Libasmir.asmir_string_of_insn prog addr)])::rest,
		 na)
      | _ -> v)
	with Disasm.Unimplemented ->
      DV.dprintf "Disassembling %Lx through VEX" addr;
      fallback()
  )

let flatten ll =
	List.rev (List.fold_left (fun accu l -> List.rev_append l accu) [] ll)

(* asmprogram_to_bap_range p st en will read bytes at [st,en) from p and translate them to bap *)
let asmprogram_to_bap_range_rich ?(init_ro = false) p st en =
  let rec f l s =
    (* This odd structure is to ensure tail-recursion *)
    let t = 
      try Some(asm_addr_to_bap p s)
      with Memory_error -> None in
    match t with
    | Some(ir, n) ->
      let l = (ir,s,n)::l in
      if n >= en then 
          List.rev l
      else
          f l n
    | None ->
      (* If we fail, hopefully it is because there were some random
    	 bytes at the end of the section that we tried to
    	 disassemble *)
      wprintf "Failed to read instruction byte while disassembling at address %#Lx; end of section at %#Lx" s en;
      List.rev l
  in
  f [] st

let asmprogram_to_bap_range ?(init_ro = false) p st en =
    let l = asmprogram_to_bap_range_rich ~init_ro:init_ro p st en in
    let l = List.map (fun (ir,_,_) -> ir) l in
    flatten l

let asmprogram_section_to_bap p s =
  let size = bfd_section_size s and vma = bfd_section_vma s in
  asmprogram_to_bap_range p vma (Int64.add vma size)

(** Translate an entire Libasmir.asm_program_t into a Vine program *)
let asmprogram_to_bap ?(init_ro=false) p =
  let irs = List.map 
	(fun s -> 
	  if is_code s then asmprogram_section_to_bap p s else []) p.secs in
  let ir = flatten irs in
  if init_ro then
  let g = gamma_for_arch p.arch in
    let m = gamma_lookup g "$mem" in
    get_rodata_assignments ~prepend_to:ir m p
  else ir

(* translate byte sequence to bap ir *)
let byte_insn_to_bap arch addr byteinsn =
  let prog = Libasmir.byte_insn_to_asmp arch addr byteinsn in
  let get a = Array.get byteinsn (Int64.to_int (Int64.sub a addr)) in
  let (pr, n) = asm_addr_to_bap {asmp=prog; arch=arch; secs=[]; get=get} addr in
  Libasmir.asmir_close prog;
  pr, Int64.sub n addr

(* Get stmts for a frame *)
let trans_frame f =
  let arch = Libbfd.Bfd_arch_i386 in
  let t = Libasmir.asmir_frame_type f in
  match t with
  | Libasmir.FRM_STD2 -> 
      let bytes, addr, _ = Libasmir.asmir_frame_get_insn_bytes f in
      (* Array.iter (fun x -> dprintf "Byte: %x" (int_of_char x)) bytes; *)
      let stmts, _ = byte_insn_to_bap arch addr bytes in
      stmts
  | Libasmir.FRM_TAINT -> 
      [Comment("ReadSyscall", []); Comment("All blocks must have two statements", [])]
  | Libasmir.FRM_LOADMOD ->
      let name, lowaddr, highaddr = Libasmir.asmir_frame_get_loadmod_info f in
      [Special(Printf.sprintf "Loaded module '%s' at %#Lx to %#Lx" name lowaddr highaddr, []); Comment("All blocks must have two statements", [])]
  | Libasmir.FRM_SYSCALL ->
	let callno, addr, tid = Libasmir.asmir_frame_get_syscall_info f in
	[Special(Printf.sprintf "Syscall number %d at %#Lx by thread %d" callno addr tid,[]);
	 Comment("All blocks must have two statements", [])]
  | Libasmir.FRM_EXCEPT ->
	let exceptno, tid, from_addr, to_addr =
	  Libasmir.asmir_frame_get_except_info f in
	[Special(Printf.sprintf "Exception number %d by thread %d at %#Lx to %#Lx" exceptno tid from_addr to_addr,[]);
	 Comment("All blocks must have two statements", [])]
  | _ -> []

(* SWXXX Add buffering around this/let it find a range where alt_bap finds entire range *)
let alt_bap_from_trace_file filename =
  let add_operands stmts f =
    let ops = tr_frame_attrs f in
    match stmts with
    | Label (l,a)::others ->
	Label (l,a@ops)::others
    | Comment (s,a)::others ->
	Comment (s,a@ops)::others
    | others when ops <> [] -> Comment("Attrs without label.", ops)::others
    | others -> others
  in
  let raise_frame f =
    let stmts = trans_frame f in
    add_operands stmts f
  in
  let ir = ref [] in
  let off = ref 0L in
  let c = ref true in
  (* might be better to just grab the length of the trace in advance :*( *)
  Status.init "Lifting trace" 0 ;
  while !c do
    (*dprintf "Calling the trace again.... ";*)
    (* flush VEX buffers *)
    let () = Libasmir.asmir_free_vex_buffers () in
    let trace_frames = Libasmir.asmir_frames_from_trace_file filename !off !trace_blocksize in
    let numframes = Libasmir.asmir_frames_length trace_frames in
    (*dprintf "Got %d frames" numframes;*)
    if numframes = 0 || numframes = -1 then (
      c := false
    ) else (
      let revstmts = Util.foldn
	(fun acc n ->
	   let frameno = numframes-1-n in
	   (* dprintf "frame %d" frameno; *)
	   let stmts = raise_frame (Libasmir.asmir_frames_get trace_frames frameno) in
	   List.rev_append stmts acc) [] (numframes-1) in
      ir := Util.fast_append revstmts !ir;

      (* let moreir = tr_bap_blocks_t_no_asm g bap_blocks in *)
	(* Build ir backwards *)
	(* ir := List.rev_append moreir !ir; *)
	off := Int64.add !off !trace_blocksize
    );

    asmir_frames_destroy trace_frames;

  done;
  let r = List.rev !ir in
  Status.stop () ;
  r

(* deprecated *)  
let old_bap_from_trace_file ?(atts = true) ?(pin = false) filename =
  let g = gamma_create x86_mem x86_regs in
  let ir = ref [] in
  let off = ref 0L in
  let c = ref true in
  (* might be better to just grab the length of the trace in advance :*( *)
  Status.init "Lifting trace" 0 ;
  while !c do
    (*dprintf "Calling the trace again.... ";*)
    let bap_blocks = Libasmir.asmir_bap_from_trace_file filename !off !trace_blocksize atts pin in
    let numblocks = Libasmir.asmir_bap_blocks_size bap_blocks in
    if numblocks = -1 then (
      c := false
    ) else (
      let moreir = tr_bap_blocks_t_trace_asm g bap_blocks in
      let () = destroy_bap_blocks bap_blocks in
	(* Build ir backwards *)
	ir := List.rev_append moreir !ir;
	off := Int64.add !off !trace_blocksize
    )
  done;
  let r = List.rev !ir in
  Status.stop () ;
  r

let bap_from_trace_file ?(atts = true) ?(pin = false) filename =
  if pin then
    alt_bap_from_trace_file filename
  else
    old_bap_from_trace_file ~atts ~pin filename

(** Get one statement at a time.

    XXX: Use an internal buffer
*)
let bap_get_stmt_from_trace_file ?(atts = true) ?(pin = false) filename off =
  let off = Int64.of_int off in (* blah, Stream.from does not use int64 *)
  let g = gamma_create x86_mem x86_regs in
  (* SWXXX this is the old way, use alt_bap_from_trace_file instead *)
  (* SWXXX this has no buffer at all; will parse entire trace for every instruction *)
  let bap_blocks = Libasmir.asmir_bap_from_trace_file filename off 1L atts pin in
  let numblocks = Libasmir.asmir_bap_blocks_size bap_blocks in
  let ir = tr_bap_blocks_t_trace_asm g bap_blocks in
  let () = destroy_bap_blocks bap_blocks in
  match numblocks with
  | -1 -> None
  | _ -> Some(ir)
  
(** Return stream of trace instructions raised to the IL *)
let bap_stream_from_trace_file ?(atts = true) ?(pin = false) filename =
  Stream.from (bap_get_stmt_from_trace_file ~atts:atts ~pin:pin filename)

let get_symbols ?(all=false) {asmp=p} =
  let f = if all then asmir_get_all_symbols else asmir_get_symbols in
  let (arr,err) = f p in
  if err <= 0 then failwith "get_symbols";
  arr

let get_function_ranges p =
  let symb = get_symbols p in
  ignore p; (* does this ensure p is live til here? *)
  let is_function s =
    s.bfd_symbol_flags land bsf_function <> 0
  and symb_to_tuple s =
    (* FIXME: section_end doesn't seem to get the right values... *)
    (* did this fix it? --aij *)
    let sec = s.bfd_symbol_section in
    let vma = bfd_section_vma sec in
    (Int64.add s.bfd_symbol_value vma,
     Int64.add vma (bfd_section_size sec),
     s.bfd_symbol_name)
  in
  let starts =
    Array.fold_left
      (fun l s -> if is_function s then symb_to_tuple s :: l else l)
      [] symb
  in
  let starts = Array.of_list starts in
  (* FIXME: probably should do unsigned comparison *)
  Array.fast_sort compare starts;
  (*let ranges = Array.mapi
    (fun i (s,e,name) ->
       let e' =
	 try let (s,_,_) = starts.(i+1) in s
	 with Invalid_argument "index out of bounds" -> e
       in
       if e' < e || e = s then (name,s,e') else (name,s,e)
    ) starts
  *)
  let ranges = Array.mapi
    (fun i (s,e,name) ->
       let e' =
	 try let (s,_,_) = starts.(i+1) in s
	 with Invalid_argument "index out of bounds" -> s
       in
       (name,s,e') (* section_end doesn't work *)
    ) starts
  in
  let unfiltered = Array.to_list ranges in
  (* filter out functions that start at 0 *)
  List.filter (function 
		 |(_,0L,_) -> false
		 |("_init",_,_) -> false
		 | _ -> true)
    unfiltered


let get_section_startaddr p sectionname =
  Libasmir.asmir_get_sec_startaddr p.asmp sectionname

let get_section_endaddr p sectionname =
  Libasmir.asmir_get_sec_endaddr p.asmp sectionname


let get_asm_instr_string_range p s e =
  let s = ref s in
  let str = ref "" in
  while !s < e do

    str := !str ^ "; " ^ (Libasmir.asmir_string_of_insn p.asmp !s);

    let len = Int64.of_int (Libasmir.asmir_get_instr_length p.asmp !s) in
    s := Int64.add !s len
  done;
  !str

let set_print_warning = Libasmir.asmir_set_print_warning

let get_print_warning = Libasmir.asmir_get_print_warning

let set_use_simple_segments = Libasmir.asmir_set_use_simple_segments

let get_prog_contents {get=get} addr =
  get addr
