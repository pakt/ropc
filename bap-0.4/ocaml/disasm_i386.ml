(** Native lifter of x86 instructions to the BAP IL *)

open Int64
open Ast
open Ast_convenience
open Big_int
open Big_int_convenience
open Type
module VH=Var.VarHash

module D = Debug.Make(struct let name = "Disasm_i386" and default=`NoDebug end)
open D

(* To help understand this file, please refer to the
   Intel Instruction Set Reference. For consistency, any section numbers
   here are wrt Order Number: 253666-035US June 2010 and 253667-035US.


  The x86 instruction format is as follows:
   Instuction Prefixexs: 0-4bytes (1 byte per prefix)
   Opcode: 1 - 3 bytes.
   ModR/M: 1 optional byte
   SIB: 1 optional byte
   Displacement: 0,1,2, or 4 bytes.
   Immediate: 0,1,2, or 4 bytes

   ModR/M has the following format:
   7:6 Mod
   5:3 Reg or extra opcode bits
   2:0 R/M

   SIB:
   7:6 Scale
   5:3 Index
   2:0 Base


   In order to get the most common unspported opcodes, you can run something like:
   for f in bin/*; do BAP_DEBUG_MODULES=AsmirV ~/bap/trunk/utils/iltrans -bin $f ; done 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n

   To optimize for number of programs disassembled:
   for f in bin/*; do echo -n "$f "; BAP_DEBUG_MODULES=AsmirV iltrans -bin $f 2>&1  >/dev/null  | grep opcode | sed 's/.*opcode: //' | sort | uniq -c | sort -n  | wc -l; done | sort -n -k 2

*)

(* type segment = CS | SS | DS | ES | FS | GS *)

type operand =
  | Oreg of int
  | Oaddr of Ast.exp
  | Oimm of int64 (* XXX: Should this be big_int? *)

type opcode =
  | Retn
  | Nop
  | Mov of typ * operand * operand (* dst, src *)
  | Movs of typ
  | Movzx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movsx of typ * operand * typ * operand (* dsttyp, dst, srctyp, src *)
  | Movdqa of operand * operand (* dst, src *)
  | Lea of operand * Ast.exp
  | Call of operand * int64 (* int64 is RA *)
  | Shift of binop_type * typ * operand * operand
  | Shiftd of binop_type * typ * operand * operand * operand
  | Bt of typ * operand * operand
  | Jump of operand
  | Jcc of operand * Ast.exp
  | Setcc of typ * operand * Ast.exp
  | Hlt
  | Cmps of typ
  | Scas of typ
  | Stos of typ
  | Push of typ * operand
  | Pop of typ * operand
  | Add of (typ * operand * operand)
  | Inc of typ * operand
  | Dec of typ * operand
  | Sub of (typ * operand * operand)
  | Sbb of (typ * operand * operand)
  | Cmp of (typ * operand * operand)
  | And of (typ * operand * operand)
  | Or of (typ * operand * operand)
  | Xor of (typ * operand * operand)
  | Pxor of (typ * operand * operand)
  | Test of typ * operand * operand
  | Not of typ * operand
  | Cld
  | Rdtsc
  | Cpuid
  | Stmxcsr of operand
  | Ldmxcsr of operand
  | Fnstcw of operand
  | Fldcw of operand
  | Leave of typ
  | Interrupt of operand

(* prefix names *)
let pref_lock = 0xf0
and repnz = 0xf2
and repz = 0xf3
and hint_bnt = 0x2e
and hint_bt = 0x3e
and pref_cs = 0x2e
and pref_ss = 0x36
and pref_ds = 0x3e
and pref_es = 0x26
and pref_fs = 0x64
and pref_gs = 0x65
and pref_opsize = 0x66
and pref_addrsize = 0x67


let unimplemented s  = failwith ("disasm_i386: unimplemented feature: "^s)

let (&) = (land)
and (>>) = (lsr)
and (<<) = (lsl)


(* register widths *)
let r1 = Ast.reg_1
let r4 = Reg 4
let r8 = Ast.reg_8
let r16 = Ast.reg_16
let r32 = Ast.reg_32
let addr_t = r32
let r64 = Ast.reg_64
let r128 = Reg 128
let xmm_t = Reg 128

let nv = Var.newvar
(* registers *)

let ebp = nv "R_EBP" r32
and esp = nv "R_ESP" r32
and esi = nv "R_ESI" r32
and edi = nv "R_EDI" r32
and eip = nv "R_EIP" r32 (* why is eip in here? *)
and eax = nv "R_EAX" r32
and ebx = nv "R_EBX" r32
and ecx = nv "R_ECX" r32
and edx = nv "R_EDX" r32
and eflags = nv "EFLAGS" r32
  (* condition flag bits *)
and cf = nv "R_CF" r1
and pf = nv "R_PF" r1
and af = nv "R_AF" r1
and zf = nv "R_ZF" r1
and sf = nv "R_SF" r1
and oF = nv "R_OF" r1

and dflag = nv "R_DFLAG" r32 (* 1 if DF=0 or -1 if DF=1 *)

and fs_base = nv "R_FS_BASE" r32
and gs_base = nv "R_GS_BASE" r32

and fpu_ctrl = nv "R_FPU_CONTROL" r16
and mxcsr = nv "R_MXCSR" r32

let xmms = Array.init 8 (fun i -> nv (Printf.sprintf "R_XMM%d" i) xmm_t)

let regs : var list =
  ebp::esp::esi::edi::eip::eax::ebx::ecx::edx::eflags::cf::pf::af::zf::sf::oF::dflag::fs_base::gs_base::fpu_ctrl::mxcsr::
  List.map (fun (n,t) -> Var.newvar n t)
    [

  (* VEX left-overs from calc'ing condition flags *)
  ("R_CC_OP", reg_32);
  ("R_CC_DEP1", reg_32);
  ("R_CC_DEP2", reg_32);
  ("R_CC_NDEP", reg_32);

  (* more status flags *)
  ("R_IDFLAG", reg_32);
  ("R_ACFLAG", reg_32);
  ("R_EMWARN", reg_32);
  ("R_LDT", reg_32);
  ("R_GDT", reg_32);

  (* segment regs *)
  ("R_CS", reg_16);
  ("R_DS", reg_16);
  ("R_ES", reg_16);
  ("R_FS", reg_16);
  ("R_GS", reg_16);
  ("R_SS", reg_16);

  (* floating point *)
  ("R_FTOP", reg_32);
  ("R_FPROUND", reg_32);
  ("R_FC3210", reg_32);
    ]
    @ Array.to_list xmms

let o_eax = Oreg 0
and o_ecx = Oreg 1
and o_edx = Oreg 2
and o_ebx = Oreg 3
and o_esp = Oreg 4
and o_ebp = Oreg 5
and o_esi = Oreg 6
and o_edi = Oreg 7

let esp_e = Var esp
and ebp_e = Var ebp
and esi_e = Var esi
and edi_e = Var edi
and ecx_e = Var ecx

let mem = nv "mem" (TMem(r32))
let mem_e = Var mem
and cf_e = Var cf
and pf_e = Var pf
and af_e = Var af
and zf_e = Var zf
and sf_e = Var sf
and of_e = Var oF

and dflag_e = Var dflag

let esiaddr = Oaddr esi_e
and ediaddr = Oaddr edi_e

let seg_cs = None
and seg_ss = None
and seg_ds = None
and seg_es = None
and seg_fs = Some fs_base
and seg_gs = Some gs_base

(* exp helpers *)

let loadm m t a =
  Load(Var m, a, little_endian, t)

let load_s s t a = match s with
  | None -> Load(mem_e, a, little_endian, t)
  | Some v -> Load(mem_e, Var v +* a, little_endian, t)

let ite t b e1 e2 =
  exp_ite ~t b e1 e2

let l32 i = Int(Arithmetic.to_big_int (big_int_of_int64 i,r32), r32)
let l16 i = Int(Arithmetic.to_big_int (big_int_of_int64 i,r16), r16)
let lt i t = Int(Arithmetic.to_big_int (big_int_of_int64 i,t), t)

let i32 i = Int(biconst i, r32)
let it i t = Int(biconst i, t)

(* converts a register number to the corresponding 32bit register variable *)
let bits2reg32= function
  | 0 -> eax
  | 1 -> ecx
  | 2 -> edx
  | 3 -> ebx
  | 4 -> esp
  | 5 -> ebp
  | 6 -> esi
  | 7 -> edi
  | _ -> failwith "bits2reg32 takes 3 bits"

let bits2xmm b = xmms.(b)

and reg2bits r = Util.list_firstindex [eax; ecx; edx; ebx; esp; ebp; esi; edi] ((==)r)

let bits2xmme b = Var(bits2xmm b)

let bits2reg32e b = Var(bits2reg32 b)

let bits2reg16e b =
  cast_low r16 (bits2reg32e b)

let bits2reg8e b =
  if b < 4 then
    cast_low r8 (bits2reg32e b)
  else
    cast_high r8 (cast_low r16 (bits2reg32e (b land 3)))

let reg2xmm r =
  bits2xmm (reg2bits r)

(* These aren't used by Disasm_i386, but might be useful to external
   users. *)
let subregs =
  let hi r = (reg2bits r) + 4 in
  (eax, "R_AL", bits2reg8e (reg2bits eax))
  :: (ecx, "R_CL", bits2reg8e (reg2bits ecx))
  :: (edx, "R_DL", bits2reg8e (reg2bits edx))
  :: (ebx, "R_BL", bits2reg8e (reg2bits ebx))
  :: (eax, "R_AH", bits2reg8e (hi eax))
  :: (ecx, "R_CH", bits2reg8e (hi ecx))
  :: (edx, "R_DH", bits2reg8e (hi edx))
  :: (ebx, "R_BH", bits2reg8e (hi ebx))
  :: (eax, "R_AX", bits2reg16e (reg2bits eax))
  :: (ecx, "R_CX", bits2reg16e (reg2bits ecx))
  :: (edx, "R_DX", bits2reg16e (reg2bits edx))
  :: (ebx, "R_BX", bits2reg16e (reg2bits ebx))
  :: []

let subregs_find =
  let h = VH.create 10 in
  let () = List.iter (fun ((fr,_,_) as t) -> VH.add h fr t) subregs in
  VH.find_all h

(* effective addresses for 16-bit addressing *)
let eaddr16 = function
  (* R/M byte *)
  | 0 -> (Var ebx) +* (Var esi)
  | 1 -> (Var ebx) +* (Var edi)
  | 2 -> (Var ebp) +* (Var esi)
  | 3 -> (Var ebp) +* (Var edi)
  | 4 -> Var esi
  | 5 -> Var edi
  | 6 -> Var ebp
  | 7 -> Var ebx
  | _ -> failwith "eaddr16 takes only 0-7"

let eaddr16e b = cast_low r16 (eaddr16 b)

module ToIR = struct

(* stmt helpers *)
let move v e =
  Move(v, e, [])

let store_s s t a e = match s with
  | None -> move mem (Store(mem_e, a, e, little_endian, t))
  | Some v -> move mem (Store(mem_e, Var v +* a, e, little_endian, t))

let storem m t a e =
  move m (Store(Var m, a, e, little_endian, t))

let op2e_s ss t = function
  | Oreg r when t = r128 -> bits2xmme r
  | Oreg r when t = r32 -> bits2reg32e r
  | Oreg r when t = r16 -> bits2reg16e r
  | Oreg r when t = r8 -> bits2reg8e r
  | Oreg r -> unimplemented "unknown register"
  | Oaddr e -> load_s ss t e
  | Oimm i -> Int(Arithmetic.to_big_int (big_int_of_int64 i,t), t)

let assn_s s t v e =
  match v with
  | Oreg r when t = r128 -> move (bits2xmm r) e
  | Oreg r when t = r32 -> move (bits2reg32 r) e
  | Oreg r when t = r16 ->
    let v = bits2reg32 r in
    move v ((Var v &* l32 0xffff0000L) |* cast_unsigned r32 e)
  | Oreg r when t = r8 && r < 4 ->
    let v = bits2reg32 r in
    move v ((Var v &* l32 0xffffff00L) |* cast_unsigned r32 e)
  | Oreg r when t = r8 ->
    let v = bits2reg32 (r land 3) in
    move v ((Var v &* l32 0xffff00ffL) |* (cast_unsigned r32 e <<* i32 8))
  | Oreg _ -> unimplemented "assignment to sub registers"
  | Oaddr a -> store_s s t a e
  | Oimm _ -> failwith "disasm_i386: Can't assign to an immediate value"

let bytes_of_width = function
  | Reg x when x land 7 = 0 -> x/8
  | _ -> failwith "bytes_of_width"

let string_incr t v =
  if t = r8 then
    move v (Var v +* dflag_e)
  else
    move v (Var v +* (dflag_e ** i32(bytes_of_width t)))

let rep_wrap ?check_zf ~addr ~next stmts =
  let endstmt = match check_zf with
    | None -> Jmp(l32 addr, [])
    | Some x when x = repz ->
      CJmp(zf_e, l32 addr, l32 next, [])
    | Some x when x = repnz ->
      CJmp(zf_e, l32 next, l32 addr, [])
    | _ -> failwith "invalid value for ?check_zf"
  in
    cjmp (ecx_e ==* l32 0L) (l32 next)
    @ stmts
    @ move ecx (ecx_e -* i32 1)
    :: cjmp (ecx_e ==* l32 0L) (l32 next)
    @ [endstmt]

let reta = [StrAttr "ret"]
and calla = [StrAttr "call"]

let compute_sf result = Cast(CAST_HIGH, r1, result)
let compute_zf t result = Int(bi0, t) ==* result
let compute_pf t r =
  (* extra parens do not change semantics but do make it pretty print nicer *)
  exp_not (Cast(CAST_LOW, r1, (((((((r >>* it 7 t) ^* (r >>* it 6 t)) ^* (r >>* it 5 t)) ^* (r >>* it 4 t)) ^* (r >>* it 3 t)) ^* (r >>* it 2 t)) ^* (r >>* it 1 t)) ^* r))

let set_sf r = move sf (compute_sf r)
let set_zf t r = move zf (compute_zf t r)
let set_pf t r = move pf (compute_pf t r)

let set_pszf t r =
  [set_pf t r;
   set_sf r;
   set_zf t r]

(* Adjust flag

   AF is set when there is a carry to or borrow from bit 4 (starting
   at 0), when considering unsigned operands. Let X_i denote bit i of
   value X.  Note that in addition, r_4 = c + [(op1_4 + op2_4) mod 2],
   where c is the carry bit from the lower four bits. Since AF = c,
   and we want to know the value of AF, we can rewrite as AF = c = r_4
   - [(op1_4 + op2_4) mod 2]. Noting that addition and subtraction mod
   2 is just xor, we can simplify to AF = r_4 xor op1_4 xor op2_4.
*)

(* Helper functions to set flags for adding *)
let set_aopszf_add t s1 s2 r =
  let bit4 = it (1 lsl 4) t in
  move af (bit4 ==* (bit4 &* ((r ^* s1) ^* s2)))
  ::move oF (cast_high r1 ((s1 =* s2) &* (s1 ^* r)))
  ::set_pszf t r

let set_flags_add t s1 s2 r =
  move cf (r <* s1)
  ::set_aopszf_add t s1 s2 r

(* Helper functions to set flags for subtracting *)
let set_aopszf_sub t s1 s2 r =
  let bit4 = it (1 lsl 4) t in
  move af (bit4 ==* ((bit4 &* ((r ^* s1) ^* s2))))
  ::move oF (Cast(CAST_HIGH, r1, (s1 ^* s2) &* (s1 ^* r) ))
  ::set_pszf t r
let set_flags_sub t s1 s2 r =
  move cf (s2 >* s1)
  ::set_aopszf_sub t s1 s2 r


let rec to_ir addr next ss pref =
  let load = load_s ss (* Need to change this if we want seg_ds <> None *)
  and op2e = op2e_s ss
  and store = store_s ss
  and assn = assn_s ss in
  function
  | Nop -> []
  | Retn when pref = [] ->
    let t = nv "ra" r32 in
    [move t (load_s seg_ss r32 esp_e);
     move esp (esp_e +* (i32 4));
     Jmp(Var t, [StrAttr "ret"])
    ]
  | Mov(t, dst,src) when pref = [] || pref = [pref_addrsize] ->
    [assn t dst (op2e t src)]
  | Movs(Reg bits as t) ->
      let stmts =
	store_s seg_es t edi_e (load_s seg_es t esi_e)
	:: string_incr t esi
	:: string_incr t edi
	:: []
      in
      if pref = [] then
	stmts
      else if pref = [repz] || pref = [repnz] then
        (* movs has only rep instruction others just considered to be rep *)
	rep_wrap ~addr ~next stmts
      else
	unimplemented "unsupported prefix for movs"
  | Movzx(t, dst, ts, src) when pref = [] ->
    [assn t dst (cast_unsigned t (op2e ts src))]
  | Movsx(t, dst, ts, src) when pref = [] ->
    [assn t dst (cast_signed t (op2e ts src))]
  | Movdqa(d,s) -> (
    let (s, al) = match s with
      | Oreg i -> bits2xmme i, []
      | Oaddr a -> load xmm_t a, [a]
      | Oimm _ -> failwith "invalid"
    in
    let (d, al) = match d with
      (* FIXME: should this be a single move with two stores? *)
      | Oreg i ->
	let r = bits2xmm i in
	move r s, []
      | Oaddr a -> store xmm_t a s, a::al
      | Oimm _ -> failwith "invalid"
    in
    (List.map (fun a -> Assert( (a &* i32 15) =* i32 0, [])) (al))
    @ [d]

  )
  | Pxor args ->
    (* Pxor is just a larger xor *)
    to_ir addr next ss pref (Xor(args))
  | Lea(r, a) when pref = [] ->
    [assn r32 r a]
  | Call(o1, ra) when pref = [] ->
    let target = op2e r32 o1 in
    if List.mem esp (Formulap.freevars target) then unimplemented "call with esp as base";
    [move esp (esp_e -* i32 4);
     store_s None r32 esp_e (l32 ra);
     Jmp(target, calla)]
  | Jump(o) ->
    [ Jmp(op2e r32 o, [])]
  | Jcc(o, c) ->
    cjmp c (op2e r32 o)
  | Setcc(t, o1, c) ->
    [assn t o1 (cast_unsigned t c)]
  | Shift(st, s, o1, o2) (*when pref = [] || pref = [pref_opsize]*) ->
    assert (List.mem s [r8; r16; r32]);
    let t1 = nv "t1" s and tmpDEST = nv "tmpDEST" s
    and bits = Arithmetic.bits_of_width s
    and s_f = match st with LSHIFT -> (<<*) | RSHIFT -> (>>*) | ARSHIFT -> (>>>*)
      | _ -> failwith "invalid shift type"
    and count = (op2e s o2) &* (it 31 s)
    and e1 = op2e s o1 in
    let ifzero = ite r1 (count ==* (it 0 s))
    and our_of = match st with
      | LSHIFT -> Cast(CAST_HIGH, r1, e1) ^* cf_e
      | RSHIFT -> Cast(CAST_HIGH, r1, Var tmpDEST)
      | ARSHIFT -> exp_false
      | _ -> failwith "imposible"
    in
    [move tmpDEST e1;
     if st = LSHIFT then
       move t1 (e1 >>* ((it bits s) -* count))
     else
       move t1 (e1 >>* (count -* (it 1 s)))
     ;
     move cf (ifzero cf_e (Cast(CAST_LOW, r1, Var t1)));
     assn s o1 (s_f e1 count);
     move oF (ifzero of_e (ite r1 (count ==* (it 1 s)) (our_of) (Unknown("OF <- undefined", r1))));
     move sf (ifzero sf_e (compute_sf e1));
     move zf (ifzero zf_e (compute_zf s e1));
     move pf (ifzero pf_e (compute_pf s e1));
     move af (ifzero af_e (Unknown("AF undefined after shift", r1)))
    ]
  | Shiftd(_st, s, dst, fill, shift) ->
      let tempDEST = nv "tempDEST" s in
      let e_dst = op2e s dst in
      let e_fill = op2e s fill in
      let e_shift = op2e s shift in
      let bits = Arithmetic.bits_of_width s in
      let our_of = cast_high r1 (Var tempDEST) ^* cast_high r1 e_dst in
      let ifzero = ite r1 (e_shift ==* it 0 s) in
      let ret1 = e_fill >>* (it bits s -* e_shift) in
      let ret2 = e_dst <<* e_shift in
      let result = ret1 |* ret2 in
      let t2 = (e_dst >>* (it bits s -* e_shift)) in
      [
        move tempDEST e_dst;
        move cf (ifzero cf_e (Cast(CAST_LOW, r1, t2)));
        assn s dst result;
        move oF (ifzero of_e (ite r1 (e_shift ==* i32 1) (our_of) (it 0 r1)));
        move sf (ifzero sf_e (compute_sf e_dst));
        move zf (ifzero zf_e (compute_zf s e_dst));
        move pf (ifzero pf_e (compute_pf s e_dst));
        move af (ifzero af_e (Unknown ("AF undefined after shift", r1)))
      ]
  | Bt(t, bitoffset, bitbase) ->
      let offset = op2e t bitoffset in
      let value, shift = match bitbase with
        | Oreg i ->
            let reg = op2e t bitbase in
            let shift = offset &* it (Arithmetic.bits_of_width t - 1) t in
            reg, shift
        | Oaddr a ->
            let byte = load r8 (a +* (offset >>* (it 3 t))) in
            let shift = (cast_low r8 offset) &* (it 7 r8) in
            byte, shift
        | Oimm _ -> failwith "Immediate bases not allowed"
      in
      [
        move cf (cast_low r1 (value >>* shift));
	move oF (Unknown ("OF undefined after bt", r1));
	move sf (Unknown ("SF undefined after bt", r1));
	move af (Unknown ("AF undefined after bt", r1));
	move pf (Unknown ("PF undefined after bt", r1))
      ]
  | Hlt ->
    [Jmp(Lab "General_protection fault", [])]
  | Rdtsc ->
      [
        move eax (Unknown ("rdtsc", r32));
        move edx (Unknown ("rdtsc", r32));
      ]
  | Cpuid ->
      let undef reg = move reg (Unknown ("cpuid", r32)) in
      List.map undef [eax; ebx; ecx; edx]
  | Stmxcsr (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> failwith "stmxcsr argument cannot be non-memory"
      in
      [
        store r32 dst (Var mxcsr);(*(Unknown ("stmxcsr", r32));*)
      ]
  | Ldmxcsr (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> failwith "ldmxcsr argument cannot be non-memory"
      in
      [
        move mxcsr (load r32 src);
      ]
  | Fnstcw (dst) ->
      let dst = match dst with
        | Oaddr addr -> addr
        | _ -> failwith "fnstcw argument cannot be non-memory"
      in
      [
        store r16 dst (Var fpu_ctrl);
      ]
  | Fldcw (src) ->
      let src = match src with
        | Oaddr addr -> addr
        | _ -> failwith "fldcw argument cannot be non-memory"
      in
      [
        move fpu_ctrl (load r16 src);
      ]
  | Cmps(Reg bits as t) ->
    let src1 = nv "src1" t and src2 = nv "src2" t and tmpres = nv "tmp" t in
    let stmts =
      move src1 (op2e t esiaddr)
      :: move src2 (op2e_s seg_es t ediaddr)
      :: move tmpres (Var src1 -* Var src2)
      :: string_incr t esi
      :: string_incr t edi
      :: set_flags_sub t (Var src1) (Var src2) (Var tmpres)
    in
    if pref = [] then
      stmts
    else if pref = [repz] || pref = [repnz] then
      rep_wrap ~check_zf:(List.hd pref) ~addr ~next stmts
    else
      unimplemented "unsupported flags in cmps"
  | Scas(Reg bits as t) ->
    let src1 = nv "src1" t and src2 = nv "src2" t and tmpres = nv "tmp" t in
    let stmts =
      move src1 (cast_low t (Var eax))
      :: move src2 (op2e_s seg_es t ediaddr)
      :: move tmpres (Var src1 -* Var src2)
      :: string_incr t edi
      :: set_flags_sub t (Var src1) (Var src2) (Var tmpres)
    in
    if pref = [] then
      stmts
    else if pref = [repz] || pref = [repnz] then
      rep_wrap ~check_zf:(List.hd pref) ~addr ~next stmts
    else
      unimplemented "unsupported flags in scas"
  | Stos(Reg bits as t) ->
    let stmts = [store_s seg_es t edi_e (op2e t (o_eax));
		 string_incr t edi]
    in
    if pref = [] then
      stmts
    else if pref = [repz] then
      rep_wrap ~addr ~next stmts
    else
      unimplemented "unsupported prefix for stos"
  | Push(t, o) ->
    let tmp = nv "t" t in (* only really needed when o involves esp *)
    move tmp (op2e t o)
    :: move esp (esp_e -* i32 (bytes_of_width t))
    :: store_s seg_ss t esp_e (Var tmp) (* FIXME: can ss be overridden? *)
    :: []
  | Pop(t, o) ->
    [assn t o (load_s seg_ss t esp_e);
     move esp (esp_e +* i32 (bytes_of_width t)) ]
  | Add(t, o1, o2) ->
    let tmp = nv "t" t in
    move tmp (op2e t o1)
    :: assn t o1 (op2e t o1 +* op2e t o2)
    :: let s1 = Var tmp and s2 = op2e t o2 and r = op2e t o1 in
       set_flags_add t s1 s2 r
  | Inc(t, o) (* o = o + 1 *) ->
    let tmp = nv "t" t in
    move tmp (op2e t o)
    :: assn t o (op2e t o +* it 1 t)
    :: set_aopszf_add t (Var tmp) (it 1 t) (op2e t o)
  | Dec(t, o) (* o = o - 1 *) ->
    let tmp = nv "t" t in
    move tmp (op2e t o)
    :: assn t o (op2e t o -* it 1 t)
    :: set_aopszf_sub t (Var tmp) (it 1 t) (op2e t o) (* CF is maintained *)
  | Sub(t, o1, o2) (* o1 = o1 - o2 *) ->
    let oldo1 = nv "t" t in
    move oldo1 (op2e t o1)
    :: assn t o1 (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (Var oldo1) (op2e t o2) (op2e t o1)
  | Sbb(t, o1, o2) ->
    let tmp = nv "t" t in
    let s1 = Var tmp and s2 = (op2e t o2) +* cast_unsigned t cf_e and r = op2e t o1 in
    move tmp r
    :: assn t o1 (r -* s2)
    (* FIXME: sanity check this *)
    ::move oF (cast_high r1 ((s1 ^* s2) &* (s1 ^* r)))
    ::move cf ((r >* s1) |* (r ==* s1 &* cf_e))
    ::move af (Unknown("AF for sbb unimplemented", r1))
    ::set_pszf t r
  | Cmp(t, o1, o2) ->
    let tmp = nv "t" t in
    move tmp (op2e t o1 -* op2e t o2)
    :: set_flags_sub t (op2e t o1) (op2e t o2) (Var tmp)
  | And(t, o1, o2) ->
    assn t o1 (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (op2e t o1)
  | Or(t, o1, o2) ->
    assn t o1 (op2e t o1 |* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after or", r1))
    :: set_pszf t (op2e t o1)
  | Xor(t, o1, o2) when o1 = o2->
    assn t o1 (Int(bi0,t))
    :: move af (Unknown("AF is undefined after xor", r1))
    :: List.map (fun v -> move v exp_true) [zf; pf]
    @  List.map (fun v -> move v exp_false) [oF; cf; sf]
  | Xor(t, o1, o2) ->
    assn t o1 (op2e t o1 ^* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after xor", r1))
    :: set_pszf t (op2e t o1)
  | Test(t, o1, o2) ->
    let tmp = nv "t" t in
    move tmp (op2e t o1 &* op2e t o2)
    :: move oF exp_false
    :: move cf exp_false
    :: move af (Unknown("AF is undefined after and", r1))
    :: set_pszf t (Var tmp)
  | Not(t, o1) ->
    [assn t o1 (exp_not (op2e t o1))]
  | Cld ->
    [Move(dflag, i32 1, [])]
  | Leave t when pref = [] -> (* #UD if Lock prefix is used *)
    Move(esp, ebp_e, [])
    ::to_ir addr next ss pref (Pop(t, o_ebp))
  | Interrupt(Oimm i) ->
    [Special(Printf.sprintf "int %Lx" i, [])]
  | _ -> unimplemented "to_ir"

let add_labels ?(asm) a ir =
  let attr = match asm with None -> [] | Some s -> [Asm(s)] in
  Label(Addr a, attr)
  ::Label(Name(Printf.sprintf "pc_0x%Lx" a),[])
  ::ir

end (* ToIR *)


module ToStr = struct

  let pref2str = function
(*  | Lock -> "lock"
  | Repnz -> "repnz"
  | Repz -> "repz"
  | Override _ | Hint_bnt | Hint_bt
  | Op_size | Mandatory_0f
  | Address_size -> failwith "finish pref2str" *)
    | _ -> unimplemented "pref2str"

  let rec prefs2str = function [] -> ""
    | x::xs -> pref2str x ^ " " ^ prefs2str xs

	  (* XXX Clean up printing here *)
  let oreg2str = function
	| 0 -> "eax"
	| 1 -> "ecx"
	| 2 -> "edx"
	| 3 -> "ebx"
	| 4 -> "exp"
	| 5 -> "ebp"
	| 6 -> "esi"
	| 7 -> "edi"
	| v -> unimplemented (Printf.sprintf "Don't know what oreg %i is." v)


  let opr = function
    | Oreg v -> oreg2str v
    | Oimm i -> Printf.sprintf "$0x%Lx" i
    | Oaddr a -> Pp.ast_exp_to_string a

  let op2str = function
    | Retn -> "ret"
    | Nop -> "nop"
    | Mov(t,d,s) -> Printf.sprintf "mov %s, %s" (opr d) (opr s)
    | Movs(t) -> "movs"
    | Movzx(dt,dst,st,src) -> Printf.sprintf "movzx %s, %s" (opr dst) (opr src)
    | Movsx(dt,dst,st,src) -> Printf.sprintf "movsx %s, %s" (opr dst) (opr src)
    | Movdqa(d,s) -> Printf.sprintf "movdqa %s, %s" (opr d) (opr s)
    | Lea(r,a) -> Printf.sprintf "lea %s, %s" (opr r) (opr (Oaddr a))
    | Call(a, ra) -> Printf.sprintf "call %s" (opr a)
    | Shift _ -> "shift"
    | Shiftd _ -> "shiftd"
    | Hlt -> "hlt"
    | Rdtsc -> "rdtsc"
    | Cpuid -> "cpuid"
    | Stmxcsr (o) -> Printf.sprintf "stmxcr %s" (opr o)
    | Ldmxcsr (o) -> Printf.sprintf "ldmxcr %s" (opr o)
    | Fnstcw (o) -> Printf.sprintf "fnstcw %s" (opr o)
    | Fldcw (o) -> Printf.sprintf "fldcw %s" (opr o)
    | Inc (t, o) -> Printf.sprintf "inc %s" (opr o)
    | Dec (t, o) -> Printf.sprintf "dec %s" (opr o)
    | Jump a -> Printf.sprintf "jmp %s" (opr a)
    | Bt(t,d,s) -> Printf.sprintf "bt %s, %s" (opr d) (opr s)
    | Jcc _ -> "jcc"
    | Setcc _ -> "setcc"
    | Cmps _ -> "cmps"
    | Scas _ -> "scas"
    | Stos _ -> "stos"
    | Push(t,o) -> Printf.sprintf "push %s" (opr o)
    | Pop(t,o) -> Printf.sprintf "pop %s" (opr o)
    | Add(t,d,s) -> Printf.sprintf "add %s, %s" (opr d) (opr s)
    | Sub(t,d,s) -> Printf.sprintf "sub %s, %s" (opr d) (opr s)
    | Sbb(t,d,s) -> Printf.sprintf "sbb %s, %s" (opr d) (opr s)
    | Cmp(t,d,s) -> Printf.sprintf "cmp %s, %s" (opr d) (opr s)
    | And(t,d,s) -> Printf.sprintf "and %s, %s" (opr d) (opr s)
    | Or(t,d,s) -> Printf.sprintf "or %s, %s" (opr d) (opr s)
    | Xor(t,d,s) -> Printf.sprintf "xor %s, %s" (opr d) (opr s)
    | Pxor(t,d,s)  -> Printf.sprintf "pxor %s, %s" (opr d) (opr s)
    | Test(t,d,s) -> Printf.sprintf "test %s, %s" (opr d) (opr s)
    | Not(t,o) -> Printf.sprintf "not %s" (opr o)
    | Cld -> "cld"
    | Leave _ -> "leave"
    | Interrupt(o) -> Printf.sprintf "int %s" (opr o)
    (*_ -> unimplemented "op2str"*)

  let to_string pref op =
    failwith "fallback to libdisasm"
    (* prefs2str pref ^ op2str op *)
end (* ToStr *)

(* extract the condition to jump on from the opcode bits
for 70 to 7f and 0f 80 to 8f *)
let cc_to_exp i =
  let cc = match i & 0xe with
    | 0x0 -> of_e
    | 0x2 -> cf_e
    | 0x4 -> zf_e
    | 0x6 -> cf_e |* zf_e
    | 0x8 -> sf_e
    | 0xc -> sf_e ^* of_e
    | 0xe -> zf_e |* (sf_e ^* of_e)
    | _ -> failwith "unsupported condition code"
  in
  if (i & 1) = 0 then cc else exp_not cc

let parse_instr g addr =
  let s = Int64.succ in

  let get_prefix c =
    let i = Char.code c in
    match i with
    | 0xf0 | 0xf2 | 0xf3 | 0x2e | 0x36 | 0x3e | 0x26 | 0x64 | 0x65
    | 0x66 | 0x67 -> Some i
    | _ -> None
  in
  let get_prefixes a =
    let rec f l a =
      match get_prefix (g a) with
      | Some p -> f (p::l) (s a)
      | None -> (l, a)
    in
    f [] a
  in
(*  let int2prefix ?(jmp=false) = function
    | 0xf0 -> Some Lock
    | 0xf2 -> Some Repnz
    | 0xf3 -> Some Repz
    | 0x2e when jmp-> Some Hint_bnt
    | 0x3e when jmp-> Some Hint_bt
    | 0x2e -> Some(Override CS)
    | 0x36 -> Some(Override SS)
    | 0x3e -> Some(Override DS)
    | 0x26 -> Some(Override ES)
    | 0x64 -> Some(Override FS)
    | 0x65 -> Some(Override GS)
    | 0x66 -> Some Op_size
    | 0x0f -> Some Mandatory_0f
    | 0x67 -> Some Address_size
    | _ -> None
  in*)
  let parse_disp8 a =
    (* This has too many conversions, but the below code doesn't handle
       signedness correctly. *)
    (int64_of_big_int (Arithmetic.to_sbig_int (big_int_of_int (Char.code (g a)), r8)), s a)
  (* let r n = Int64.shift_left (Int64.of_int (Char.code (g (Int64.add a (Int64.of_int n))))) (8*n) in *)
  (*   let d = r 0 in *)
  (*   (d, (Int64.succ a)) *)
  and parse_disp16 a =
    (* XXX: Does this handle negative displacements correctly? *)
    let r n = Int64.shift_left (Int64.of_int (Char.code (g (Int64.add a (Int64.of_int n))))) (8*n) in
    let d = r 0 in
    let d = Int64.logor d (r 1) in
    (d, (Int64.add a 2L))
  and parse_disp32 a =
    let r n = Int64.shift_left (Int64.of_int (Char.code (g (Int64.add a (Int64.of_int n))))) (8*n) in
    let d = r 0 in
    let d = Int64.logor d (r 1) in
    let d = Int64.logor d (r 2) in
    let d = Int64.logor d (r 3) in
    (d, (Int64.add a 4L))
  in
  let parse_disp:(Type.typ -> int64 -> int64 * int64) = function
    | Reg 8 ->  parse_disp8
    | Reg 16 -> parse_disp16
    | Reg 32 -> parse_disp32
    | _ -> failwith "unsupported displacement size"
  in
  let parse_sib m a =
    (* ISR 2.1.5 Table 2-3 *)
    let b = Char.code (g a) in
    let ss = b >> 6 and idx = (b>>3) & 7 in
    let base, na = if (b & 7) <> 5 then (bits2reg32e (b & 7), s a) else
	match m with
	| 0 -> let (i,na) = parse_disp32 (s a) in (l32 i, na)
	| _ -> unimplemented "sib ebp +? disp"
    in
    if idx = 4 then (base, na) else
      let idx = bits2reg32e idx in
      if ss = 0 then (base +* idx, na)
      else (base +* (idx <<* i32 ss), na)
  in
  let parse_modrm16ext a =
    (* ISR 2.1.5 Table 2-1 *)
    let b = Char.code (g a)
    and na = s a in
    let r = (b>>3) & 7
    and m = b >> 6
    and rm = b & 7 in
    match m with (* MOD *)
    | 0 -> (match rm with
      | 6 -> let (disp, na) = parse_disp16 (s a) in (r, Oaddr(l16 disp), na)
      | n when n < 8 -> (r, Oaddr(eaddr16 rm), s a)
      | _ -> failwith "Impossible"
    )
    | 1 | 2 ->
      let (base, na) = eaddr16 rm, na in
      let (disp, na) = if m = 1 then parse_disp8 na else (*2*) parse_disp16 na in
      (r, Oaddr(base +* l16 disp), na)
    | 3 -> (r, Oreg rm, s a)
    | _ -> failwith "Impossible"
  in
  let parse_modrm16 a =
    let (r, rm, na) = parse_modrm16ext a in
    (Oreg r, rm, na)
  in
  let parse_modrm32ext a =
    (* ISR 2.1.5 Table 2-2 *)
    let b = Char.code (g a)
    and na = s a in
    let r = (b>>3) & 7
    and m = b >> 6
    and rm = b & 7 in
    match m with (* MOD *)
    | 0 -> (match rm with
      | 4 -> let (sib, na) = parse_sib m (s a) in (r, Oaddr sib, na)
      | 5 -> let (disp, na) = parse_disp32 (s a) in (r, Oaddr(l32 disp), na)
      | n -> (r, Oaddr(bits2reg32e n), s a)
    )
    | 1 | 2 ->
      let (base, na) = if 4 = rm then parse_sib m na else (bits2reg32e rm, na) in
      let (disp, na) = if m = 1 then parse_disp8 na else (*2*) parse_disp32 na in
      (r, Oaddr(base +* l32 disp), na)
    | 3 -> (r, Oreg rm, s a)
    | _ -> failwith "Impossible"
  in
  let parse_modrm32 a =
    let (r, rm, na) = parse_modrm32ext a in
    (Oreg r, rm, na)
(*  and parse_modrmxmm a =
    let (r, rm, na) = parse_modrm32ext a in
    let rm = match rm with Oreg r -> Oreg (reg2xmm r) | _ -> rm in
    (Oreg(bits2xmm r), rm, na) *)
  in
  let parse_modrm opsize a = parse_modrm32 a in
  let parse_imm8 a = (* not sign extended *)
    (Oimm(Int64.of_int (Char.code (g a))), s a)
  and parse_simm8 a = (* sign extended *)
    let (d, na) = parse_disp8 a in
    (Oimm d, na)
  and parse_imm32 a =
    let (l,na) = parse_disp32 a in
    (Oimm l, na)
  in
  let parse_immz t a = match t with
    | Reg 8 -> parse_imm8 a (* t=r8 when operand is Ib rather than Iz *)
    | Reg 16 -> failwith "parse_imm16 a"
    | Reg 32 | Reg 64 -> parse_imm32 a
    | _ -> failwith "parse_immz unsupported size"
  in
  let parse_immv = parse_immz in (* until we do amd64 *)
  let get_opcode pref (opsize,mopsize) a =
    (* We should rename these, since the 32 at the end is misleading. *)
    let parse_disp32, parse_modrm32, parse_modrm32ext =
      if List.mem pref_addrsize pref then
        parse_disp16, parse_modrm16, parse_modrm16ext
      else parse_disp32, parse_modrm32, parse_modrm32ext
    in
    let b1 = Char.code (g a)
    and na = s a in
    match b1 with (* Table A-2 *)
	(*** most of 00 to 3d are near the end ***)
    | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 ->
		(Inc(opsize, Oreg(b1 & 7)), na)
    | 0x48 | 0x49 | 0x4a | 0x4b | 0x4c | 0x4d | 0x4e | 0x4f ->
      (Dec(opsize, Oreg(b1 & 7)), na)
    | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 ->
      (Push(opsize, Oreg(b1 & 7)), na)
    | 0x58 | 0x59 | 0x5a | 0x5b | 0x5c | 0x5d | 0x5e | 0x5f ->
      (Pop(opsize, Oreg(b1 & 7)), na)
    | 0x68 (* | 0x6a *) ->
      let (o, na) = if b1=0x68 then parse_immz opsize na else parse_simm8 na in
      (Push(opsize, o), na)
    | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79
    | 0x7c | 0x7d | 0x7e
    | 0x7f -> let (i,na) = parse_disp8 na in
	      (Jcc(Oimm(Int64.add i na), cc_to_exp b1), na)
    | 0xc3 -> (Retn, na)
    | 0xc9 -> (Leave opsize, na)
    | 0x80 | 0x81 | 0x82
    | 0x83 -> let (r, rm, na) = parse_modrm32ext na in
	      let (o2, na) =
		(* for 0x83, imm8 needs to be sign extended *)
		if b1 = 0x81 then parse_immz opsize na else parse_simm8 na
	      in
	      let opsize = if b1 land 1 = 0 then r8 else opsize in
	      (match r with (* Grp 1 *)
	      | 0 -> (Add(opsize, rm, o2), na)
	      | 4 -> (And(opsize, rm, o2), na)
	      | 5 -> (Sub(opsize, rm, o2), na)
	      | 7 -> (Cmp(opsize, rm, o2), na)
	      | _ -> unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
	      )
    | 0x84
    | 0x85 -> let (r, rm, na) = parse_modrm32 na in
	      let o = if b1 = 0x84 then r8 else opsize in
	      (Test(o, rm, r), na)
    | 0x88 -> let (r, rm, na) = parse_modrm r8 na in
	      (Mov(r8, rm, r), na)
    | 0x89 ->
      let (r, rm, na) = parse_modrm32 na in
      (Mov(opsize, rm, r), na)
    | 0x8a -> let (r, rm, na) = parse_modrm r8 na in
	      (Mov(r8, r, rm), na)
    | 0x8b -> let (r, rm, na) = parse_modrm32 na in
	      (Mov(opsize, r, rm), na)
    | 0x8d -> let (r, rm, na) = parse_modrm opsize na in
	      (match rm with
	      | Oaddr a -> (Lea(r, a), na)
	      | _ -> failwith "invalid lea (must be address)"
	      )
    | 0x90 -> (Nop, na)
    | 0xa1 ->
      let (addr, na) = parse_disp32 na in
      (Mov(opsize, o_eax, Oaddr(l32 addr)), na)
    | 0xa3 -> let (addr, na) = parse_disp32 na in
	      (Mov(opsize, Oaddr(l32 addr), o_eax), na)
    | 0xa4 -> (Movs r8, na)
    | 0xa5 -> (Movs opsize, na)
    | 0xa6 -> (Cmps r8, na)
    | 0xa7 -> (Cmps opsize, na)
    | 0xae -> (Scas r8, na)
    | 0xaf -> (Scas opsize, na)
    | 0xa8 -> let (i, na) = parse_imm8 na in
	      (Test(r8, o_eax, i), na)
    | 0xa9 -> let (i,na) = parse_immz opsize na in
	      (Test(opsize, o_eax, i), na)
    | 0xaa -> (Stos r8, na)
    | 0xab -> (Stos opsize, na)
    | 0xb0 | 0xb1 | 0xb2 | 0xb3 | 0xb4 | 0xb5 | 0xb6
    | 0xb7 -> let (i, na) = parse_imm8 na in
	      (Mov(r8, Oreg(b1 & 7), i), na)
    | 0xb8 | 0xb9 | 0xba | 0xbb | 0xbc | 0xbd | 0xbe
    | 0xbf -> let (i, na) = parse_immv opsize na in
	      (Mov(opsize, Oreg(b1 & 7), i), na)
    | 0xc6
    | 0xc7 -> let t = if b1 = 0xc6 then r8 else opsize in
	      let (e, rm, na) = parse_modrm32ext na in
              if e<>0 then failwith "Invalid opcode";
	      (* assert (e=0); (\* others are invalid opcodes, so we should check *\) *)
	      let (i,na) = parse_immz t na in
	      (match e with (* Grp 11 *)
	      | 0 -> (Mov(t, rm, i), na)
	      | _ -> failwith "invalid opcode"
	      )
    | 0xcd -> let (i,na) = parse_imm8 na in
	      (Interrupt(i), na)
    | 0xd9 ->
        let (r, rm, na) = parse_modrm32ext na in
        (match r with
           | 5 -> (Fldcw rm, na)
           | 7 -> (Fnstcw rm, na)
           | _ -> unimplemented (Printf.sprintf "unsupported opcode: d9/%d" r)
        )
    | 0xe8 -> let (i,na) = parse_disp32 na in
	      (Call(Oimm(Int64.add i na), na), na)
    | 0xe9 -> let (i,na) = parse_disp opsize na in
	      (Jump(Oimm(Int64.add i na)), na)
    | 0xeb -> let (i,na) = parse_disp8 na in
	      (Jump(Oimm(Int64.add i na)), na)
    | 0xc0 | 0xc1
    | 0xd0 | 0xd1 | 0xd2
    | 0xd3 -> let (r, rm, na) = parse_modrm32ext na in
	      let opsize = if (b1 & 1) = 0 then r8 else opsize in
	      let (amt, na) = match b1 & 0xfe with
		| 0xc0 -> parse_imm8 na
		| 0xd0 -> (Oimm 1L, na)
		| 0xd2 -> (o_ecx, na)
		| _ -> failwith "impossible"
	      in
	      (match r with (* Grp 2 *)
	      | 4 -> (Shift(LSHIFT, opsize, rm, amt), na)
	      | 5 -> (Shift(RSHIFT, opsize, rm, amt), na)
	      | 7 -> (Shift(ARSHIFT, opsize, rm, amt), na)
	      | _ -> unimplemented "Grp 2: rolls"
	      )
    | 0xf4 -> (Hlt, na)
    | 0xf6
    | 0xf7 -> let t = if b1 = 0xf6 then r8 else opsize in
	      let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 3 *)
	       | 0 -> let (imm, na) = parse_immz t na in (Test(t, rm, imm), na)
	       | 2 -> (Not(t, rm), na)
	       | _ -> unimplemented (Printf.sprintf "unsupported opcode: %02x/%d" b1 r)
	      )
    | 0xfc -> (Cld, na)
    | 0xff -> let (r, rm, na) = parse_modrm32ext na in
	      (match r with (* Grp 5 *)
	      | 2 -> (Call(rm, na), na)
	      | 4 -> (Jump rm, na)
	      | 6 -> (Push(opsize, rm), na)
	      | _ -> unimplemented (Printf.sprintf "unsupported opcode: ff/%d" r)
	      )
    | b1 when b1 < 0x3e && (b1 & 7) < 6 ->
      (
	let ins a = match b1 >> 3 with
	  | 0 -> Add a
	  | 1 -> Or a
	  (*| 2 -> Adc a*)
	  | 3 -> Sbb a
	  | 4 -> And a
	  | 5 -> Sub a
	  | 6 -> Xor a
	  | 7 -> Cmp a
	  | _ -> unimplemented (Printf.sprintf "unsupported opcode: %02x" b1)
(*	  | _ -> failwith "impossible" *)
	in
	let t = if (b1 & 1) = 0  then r8 else opsize in
	let (o1, o2, na) = match b1 & 6 with
	  | 0 -> let r, rm, na = parse_modrm t na in
		 (rm, r, na)
	  | 2 -> let r, rm, na = parse_modrm t na in
		 (r, rm, na)
	  | 4 -> let i, na = parse_immz t na in
		 (o_eax, i, na)
	  | _ -> failwith "impossible"
	in
	(ins(t, o1, o2), na)
      )
    | 0x0f -> (
      let b2 = Char.code (g na) and na = s na in
      match b2 with (* Table A-3 *)
      | 0x1f -> (Nop, na)
      | 0x31 -> (Rdtsc, na)
      | 0x6f | 0x7f when pref = [0x66] ->
            (
	      let r, rm, na = parse_modrm32 na in
	      let s,d = if b2 = 0x6f then rm, r else r, rm in
	        (Movdqa(d,s), na)
            )
      | 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87 | 0x88 | 0x89
      | 0x8c | 0x8d | 0x8e
      | 0x8f ->	let (i,na) = parse_disp32 na in
		(Jcc(Oimm(Int64.add i na), cc_to_exp b2), na)
    (* add other opcodes for setcc here *)
      | 0x94
      | 0x95 -> let r, rm, na = parse_modrm r8 na in
		assert (opsize = r32);  (* unclear what happens otherwise *)
		(Setcc(r8, rm, cc_to_exp b2), na)
      | 0xa2 -> (Cpuid, na)
      | 0xa3 | 0xba ->
          let (r, rm, na) = parse_modrm opsize na in
          let r, na = if b2 = 0xba then parse_imm8 na else r, na in
          (Bt(opsize, r, rm), na)
      | 0xa4 ->
	(* shld *)
        let (r, rm, na) = parse_modrm opsize na in
	let (i, na) = parse_imm8 na in
	(Shiftd(LSHIFT, opsize, rm, r, i), na)
      | 0xa5 ->
	(* shld *)
        let (r, rm, na) = parse_modrm opsize na in
	(Shiftd(LSHIFT, opsize, rm, r, o_ecx), na)
      | 0xae ->
          let (r, rm, na) = parse_modrm32ext na in
          (match r with
             | 2 -> (Ldmxcsr rm, na) (* ldmxcsr *)
             | 3 -> (Stmxcsr rm, na) (* stmxcsr *)
             | _ -> unimplemented (Printf.sprintf "unsupported opcode: ff/%d" r)
          )
      | 0xb6
      | 0xb7 -> let st = if b2 = 0xb6 then r8 else r16 in
		let r, rm, na = parse_modrm32 na in
		(Movzx(opsize, r, st, rm), na)
      | 0xbe
      | 0xbf -> let st = if b2 = 0xbe then r8 else r16 in
		let r, rm, na = parse_modrm32 na in
		(Movsx(opsize, r, st, rm), na)
      | 0xef ->
		let d, s, na = parse_modrm32 na in
		(Pxor(mopsize,d,s), na)
      | _ -> unimplemented (Printf.sprintf "unsupported opcode: %02x %02x" b1 b2)
    )
    | n -> unimplemented (Printf.sprintf "unsupported opcode: %02x" n)

  in
  let pref, a = get_prefixes addr in
  (* Opsize for regular instructions, MMX/SSE2 instructions

     The opsize override makes regular operands smaller, but MMX
     operands larger.  *)
  let opsize,mopsize = if List.mem pref_opsize pref then r16,r128 else r32,r64 in
  let op, a = get_opcode pref (opsize,mopsize) a in
(pref, op, a)

let parse_prefixes pref op =
  (* FIXME: how to deal with conflicting prefixes? *)
  let rec f t s r = function
    | [] -> (t, s, List.rev r)
    | 0x2e::p -> f t seg_cs r p
    | 0x36::p -> f t seg_ss r p
    | 0x3e::p -> f t seg_ds r p
    | 0x26::p -> f t seg_es r p
    | 0x64::p -> f t seg_fs r p
    | 0x65::p -> f t seg_gs r p
    | 0xf0::p -> f t s r p (* discard lock prefix *)
    | 0x66::p -> f r16 s r p
    | p::ps -> f t s (p::r) ps
  in
  f r32 None [] pref

(* address, prefixes, opcode, bap ir *)
type rich = RichAsm of int64 * int list * opcode * Ast.stmt list 

let get_meta g addr = 
  let (pref, op, na) = parse_instr g addr in
  let (_, ss, pref) =  parse_prefixes pref op in
  let asm = try Some(ToStr.to_string pref op) with Failure _ -> None in
  (pref, ss, op, asm, na)

let disasm_instr_rich g addr =
  let (pref, ss, op, asm, na) = get_meta g addr in
  let ir = ToIR.to_ir addr na ss pref op in
  let (stmts, na) = (ToIR.add_labels ?asm addr ir, na) in
  (RichAsm(addr, pref, op, stmts), na)

let disasm_instr g addr =
    let RichAsm(_,_,_,stmts), na = disasm_instr_rich g addr in
    (stmts, na)

let rich_eq r1 r2 = 
    let RichAsm(_, pref1, op1, _) = r1 in        
    let RichAsm(_, pref2, op2, _) = r2 in        
    pref1 = pref2 && op1 = op2

(*
let disasm_range g st en = 
    let rec aux l addr = 
        let (rich, n) = disasm_instr_rich g addr in
        let l = rich::l in
        if n >= en then
            l
*)
