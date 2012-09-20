(** General disassembly stuff *)

exception Unimplemented

let disasm_instr arch =
  match arch with
  | Libbfd.Bfd_arch_i386 -> Disasm_i386.disasm_instr
  | _ -> raise Unimplemented

(** is_temp v returns true iff v is used as a temporary, based on its
    name.  A temporary is a variable introduced by BAP's lifting process
    that is only referenced inside one assembly block. 

    The evaluator uses this information to throw away any state stored
    for these temporaries once control passes out of an assembly block.
*)
let is_temp (Var.V(_, s, t)) =
  (* First try VEX style vars *)
  ((String.length s > 2) && (String.sub s 0 2 = "T_"))
  || s = "ra"
  || s = "t1"
  || s = "tmpDEST"
  || s = "src1"
  || s = "src2"
  || s = "tmp"
  || s = "t"
    
