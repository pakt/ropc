(** High level interface to libasmir.
    
    The functions in this file should be used instead of calling Libasmir functions
    directly. These functions should be easier to use, and, unlike the Libasmir
    ones, will handle garbage collection.

    @author Ivan Jager
*)

(* This interface is a work-in-progress. I just created it now to avoid exposing
   some variables I added. --aij
*)

open Libbfd
open Libasmir

exception Disassembly_error

type asmprogram

type arch
val arch_i386 : arch
val arch_arm : arch

val always_vex : bool ref

type varctx

val gamma_create : Var.t -> Var.t list -> varctx
val gamma_lookup : varctx -> string -> Var.t

(*
val gamma_extend : varctx -> string -> Ast.decl -> unit
val gamma_unextend : varctx -> string -> unit
*)

(*
val tr_exp : varctx -> Libasmir.exp -> Ast.exp
val tr_binop :
  varctx ->
  Libasmir.binop_type_t -> Libasmir.exp -> Libasmir.exp -> Ast.exp
val tr_vardecl : varctx -> Libasmir.stmt -> Var.t * (unit -> unit)
val tr_vardecls :
  varctx -> Libasmir.stmt list -> Var.t list * (unit -> unit)
val tr_stmt : varctx -> Libasmir.stmt -> Ast.stmt
val tr_bap_block_t :
  varctx -> asmprogram -> Libasmir.bap_block_t -> Ast.stmt list
val tr_bap_blocks_t :
  varctx ->
  asmprogram -> Libasmir.bap_blocks_t -> Ast.stmt list
*)


val decls_for_arch : arch -> Ast.var list
val gamma_for_arch : arch -> varctx

val get_asmprogram_arch : asmprogram -> arch

val x86_mem : Var.t
val x86_regs : Var.t list

val all_regs : Var.t list

val open_program :  string -> asmprogram
val asmprogram_to_bap : ?init_ro:bool -> asmprogram -> Ast.program
val asm_addr_to_bap : (*varctx ->*) asmprogram -> address_t -> Ast.program * address_t

val asmprogram_to_bap_range_rich : ?init_ro:bool -> asmprogram -> address_t ->
    address_t  -> (Ast.program * address_t * address_t) list
val asmprogram_to_bap_range : ?init_ro:bool -> asmprogram -> address_t -> address_t  -> Ast.program

val bap_from_trace_file : ?atts:bool -> ?pin:bool -> string -> Ast.program

val bap_stream_from_trace_file : ?atts:bool -> ?pin:bool -> string -> (Ast.stmt list) Stream.t

val get_symbols : ?all:bool -> asmprogram -> asymbol array

val get_function_ranges : asmprogram -> (string * address_t * address_t) list

val get_all_asections : asmprogram -> section_ptr array

val get_section_startaddr : asmprogram -> string -> address_t
val get_section_endaddr : asmprogram -> string -> address_t

val get_asm_instr_string_range : asmprogram -> address_t -> address_t -> string

val is_load : section_ptr -> bool
val is_code : section_ptr -> bool

val byte_insn_to_bap : 
  bfd_architecture -> address_t -> char array -> Ast.program * int64

val set_print_warning : bool -> unit

val get_print_warning : unit -> bool

val set_use_simple_segments : bool -> unit

val get_prog_contents : asmprogram -> int64 -> char

(* added *)
val get_section_start : section_ptr -> address_t
val get_section_end : section_ptr -> address_t
val is_rw_data : section_ptr -> bool
val is_bss : section_ptr -> bool
