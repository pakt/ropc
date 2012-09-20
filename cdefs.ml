open Printf
open Common
open Ast
(* Types for ropc/analysis *)

type direction = Forward | Backward
(* offsets of labels can't be computed early, so use a marker to describe them.
 * offset from begining of the function/payload *)
type symb_simple = 
    (* local labels are prefixed with func name, so all searches can be global *)
    | Named of id 
    | Unnamed of direction (* always local *)

(* (start, end) - what's the distance from label "start" to label "end" ? *)
type symb = 
    | FromTo of symb_simple * symb_simple

(* symbolic(id), concrete(reg) *)
type sreg = S of int | C of reg

type instr = 
(* T0 *)
    | AdvanceStack of int
    | RawHex of int
    | MovRegConst of sreg * int (* sreg <- const *)
    | MovRegReg of sreg * sreg (* dst <- src *)
    | MovRegSymb of sreg * symb
    | WriteM of sreg * sreg (* [addr_reg] <- src *)
    | ReadM of sreg * sreg (* dst <- [addr_reg] *)
    | SaveFlags
    | OpStack of operator * sreg
    | BinO of sreg * sreg * operator * sreg
(* T1 *)
    | ReadMConst of sreg * int (* dst <- [const] *)
    | WriteMConst of int * sreg (* [const] <- src *)
    (* dst_reg <- stack_frame+off, off is precalc. during compilation *)
(* T2 *)
    | LocalAddr of int * sreg 
    | PushReg of sreg (* push sreg on emu stack *)
    | PopReg of sreg (* pop sreg from emu stack *)
(* T3 *)
    | ReadLocal of int * sreg (* dst <- local_var(i) *)
    | WriteLocal of int * sreg (* local_var(i) <- src *)
    | Lbl of id
    | Comment of string (* store deleted instructions as comments *)

type ityp = T0 | T1 | T2 | T3

let is_lbl_or_comment instr = 
    match instr with
    | Lbl(_) | Comment(_) -> true
    | _ -> false

let dump_dir = function
    | Forward -> "@f"
    | Backward -> "@b"
let dump_symb = function
    | Named(id) -> sprintf "Named(%s)" id
    | Unnamed(direction) -> sprintf "Unnamed(%s)" (dump_dir direction)

let dump_sreg = function
    | S(id) -> sprintf "r%d" (id)
    | C(r) -> (dump_reg r)

let dump_instr = function
    | AdvanceStack(n) -> sprintf "esp += %d" n
    | RawHex(n) -> sprintf "hex(0x%08x)" n
    | MovRegConst(r,c) -> sprintf "%s = 0x%08x" (dump_sreg r) c
    | MovRegReg(r1,r2) -> sprintf "%s = %s" (dump_sreg r1) (dump_sreg r2)
    | MovRegSymb(r,FromTo(s,f)) -> sprintf "%s = (from: %s, to: %s)" (dump_sreg r) (dump_symb s) (dump_symb f)
    | WriteM(r1,r2) -> sprintf "[%s] = %s" (dump_sreg r1) (dump_sreg r2)
    | ReadM(r1,r2) -> sprintf "%s = [%s]" (dump_sreg r1) (dump_sreg r2)
    | SaveFlags -> "SaveFlags"
    | OpStack(op, r) -> sprintf "esp = esp %s %s" (dump_op op) (dump_sreg r)
    | BinO(ro,r1,op,r2) -> 
            sprintf "%s = %s %s %s" (dump_sreg ro) (dump_sreg r1) (Ast.dump_op op)
            (dump_sreg r2)

    | ReadMConst(r,addr) -> sprintf "%s = [0x%08x]" (dump_sreg r) addr
    | WriteMConst(addr,r) -> sprintf "[0x%08x] = %s" addr (dump_sreg r)

    | LocalAddr(off,r) -> sprintf "%s = &local(%d)" (dump_sreg r) off
    | PushReg(r) -> sprintf "push(%s)" (dump_sreg r)
    | PopReg(r) -> sprintf "pop(%s)" (dump_sreg r)

    | ReadLocal(off,r) -> sprintf "%s = *local(%d)" (dump_sreg r) off
    | WriteLocal(off,r) -> sprintf "*local(%d) = %s" off (dump_sreg r)
    | Lbl(id) -> sprintf "%s:" id
    | Comment(s) -> sprintf ";%s" s

let ast_op_to_gadget_op op = 
    match op with
    | Add -> ADD | Sub -> SUB | Mul -> MUL | Div -> DIV | Xor -> XOR | And
    -> AND | Or -> OR 
    | Not -> failwith "'Not x' should be: x xor 0xffffffff"

let make_generator f = 
    let r = ref 0 in
    let next () = 
        let id = !r in
        let _ = r := !r + 1 in
        f id
    in
    next

let make_reg_generator () = make_generator (fun id -> S(id))

module RegOrder = struct
    type t = reg
    let compare = Pervasives.compare
end

module RegSet = Set.Make( RegOrder )

module SRegOrder = struct
    type t = sreg
    let compare = Pervasives.compare
end

module SRegSet = Set.Make( SRegOrder )

let set_from_list l = 
    let f set x = RegSet.add x set in
    List.fold_left f RegSet.empty l

(* FIXME *)
let sreg_set_from_list l = 
    let f set x = SRegSet.add x set in
    List.fold_left f SRegSet.empty l

let common_reg_set_to_sreg_set set = 
    let l = RegSet.elements set in
    let l = List.map (fun r -> C(r)) l in
    let set = sreg_set_from_list l in
    set

let dump_sreg_set set = 
    let sregs = SRegSet.elements set in
    let _ = Common.generic_dumper (fun r -> dump_sreg r) sregs in
    ()

let fULL_REG_SET = set_from_list Common.rEGS_NO_ESP


