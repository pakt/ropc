(* File generated from libasmir.idl *)

type exp
and stmt
and asm_program_t
and bap_block_t
and bap_blocks_t
and cval_t
and trace_attrs
and cval_vec_t
and memory_cell_data_t
and memory_data_t
and trace_frames_t
and trace_frame_t
and big_val_t
and threadid_t = int
and frame_type_t =
  | FRM_NONE
  | FRM_KEY
  | FRM_STD
  | FRM_LOADMOD
  | FRM_SYSCALL
  | FRM_TAINT
  | FRM_STD2
  | FRM_EXCEPT
and exp_type_t =
  | BINOP
  | UNOP
  | CONSTANT
  | MEM
  | TEMP
  | PHI
  | CAST
  | NAME
  | UNKNOWN
  | LET
  | EXTENSION
and reg_t =
  | REG_1
  | REG_8
  | REG_16
  | REG_32
  | REG_64
and binop_type_t =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | LROTATE
  | RROTATE
  | LOGICAND
  | LOGICOR
  | BITAND
  | BITOR
  | XOR
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | SDIVIDE
  | SMOD
and unop_type_t =
  | NEG
  | NOT
and const_val_t = int64
and cast_t =
  | CAST_UNSIGNED
  | CAST_SIGNED
  | CAST_HIGH
  | CAST_LOW
  | CAST_FLOAT
  | CAST_INTEGER
  | CAST_RFLOAT
  | CAST_RINTEGER
and stmt_type_t =
  | JMP
  | CJMP
  | SPECIAL
  | MOVE
  | COMMENT
  | LABEL
  | EXPSTMT
  | VARDECL
  | CALL
  | RETURN
  | FUNCTION
  | ASSERT
and cval_type_t =
  | NONE
  | BOOL
  | CHR
  | INT_16
  | INT_32
  | INT_64
  | INT_128

external asmir_get_symbols : asm_program_t -> Libbfd.asymbol array * int
	= "camlidl_libasmir_asmir_get_symbols"

external asmir_get_all_symbols : asm_program_t -> Libbfd.asymbol array * int
	= "camlidl_libasmir_asmir_get_all_symbols"

external asmir_get_all_sections : asm_program_t -> Libbfd.section_ptr array * int
	= "camlidl_libasmir_asmir_get_all_sections"

external exp_type : exp -> exp_type_t
	= "camlidl_libasmir_exp_type"

external binop_type : exp -> binop_type_t
	= "camlidl_libasmir_binop_type"

external binop_lhs : exp -> exp
	= "camlidl_libasmir_binop_lhs"

external binop_rhs : exp -> exp
	= "camlidl_libasmir_binop_rhs"

external unop_type : exp -> unop_type_t
	= "camlidl_libasmir_unop_type"

external unop_subexp : exp -> exp
	= "camlidl_libasmir_unop_subexp"

external mem_addr : exp -> exp
	= "camlidl_libasmir_mem_addr"

external mem_regtype : exp -> reg_t
	= "camlidl_libasmir_mem_regtype"

external constant_val : exp -> const_val_t
	= "camlidl_libasmir_constant_val"

external constant_regtype : exp -> reg_t
	= "camlidl_libasmir_constant_regtype"

external phi_phiname : exp -> string
	= "camlidl_libasmir_phi_phiname"

external phi_numnodes : exp -> int
	= "camlidl_libasmir_phi_numnodes"

external phi_nodeat : exp -> int -> exp
	= "camlidl_libasmir_phi_nodeat"

external temp_regtype : exp -> reg_t
	= "camlidl_libasmir_temp_regtype"

external temp_name : exp -> string
	= "camlidl_libasmir_temp_name"

external unknown_str : exp -> string
	= "camlidl_libasmir_unknown_str"

external unknown_regtype : exp -> reg_t
	= "camlidl_libasmir_unknown_regtype"

external cast_width : exp -> reg_t
	= "camlidl_libasmir_cast_width"

external cast_casttype : exp -> cast_t
	= "camlidl_libasmir_cast_casttype"

external cast_subexp : exp -> exp
	= "camlidl_libasmir_cast_subexp"

external name_string : exp -> string
	= "camlidl_libasmir_name_string"

external let_var : exp -> exp
	= "camlidl_libasmir_let_var"

external let_exp : exp -> exp
	= "camlidl_libasmir_let_exp"

external let_in : exp -> exp
	= "camlidl_libasmir_let_in"

external stmt_type : stmt -> stmt_type_t
	= "camlidl_libasmir_stmt_type"

external move_lhs : stmt -> exp
	= "camlidl_libasmir_move_lhs"

external move_rhs : stmt -> exp
	= "camlidl_libasmir_move_rhs"

external label_string : stmt -> string
	= "camlidl_libasmir_label_string"

external special_string : stmt -> string
	= "camlidl_libasmir_special_string"

external comment_string : stmt -> string
	= "camlidl_libasmir_comment_string"

external jmp_target : stmt -> exp
	= "camlidl_libasmir_jmp_target"

external cjmp_cond : stmt -> exp
	= "camlidl_libasmir_cjmp_cond"

external cjmp_ttarget : stmt -> exp
	= "camlidl_libasmir_cjmp_ttarget"

external cjmp_ftarget : stmt -> exp
	= "camlidl_libasmir_cjmp_ftarget"

external expstmt_exp : stmt -> exp
	= "camlidl_libasmir_expstmt_exp"

external vardecl_name : stmt -> string
	= "camlidl_libasmir_vardecl_name"

external vardecl_type : stmt -> reg_t
	= "camlidl_libasmir_vardecl_type"

external stmt_attributes : stmt -> trace_attrs
	= "camlidl_libasmir_stmt_attributes"

external trace_tid : trace_attrs -> threadid_t
	= "camlidl_libasmir_trace_tid"

external conc_map_size : trace_attrs -> int
	= "camlidl_libasmir_conc_map_size"

external get_cval : trace_attrs -> int -> cval_t
	= "camlidl_libasmir_get_cval"

external cval_name : cval_t -> string
	= "camlidl_libasmir_cval_name"

external cval_value : cval_t -> big_val_t
	= "camlidl_libasmir_cval_value"

external cval_value_size : big_val_t -> int
	= "camlidl_libasmir_cval_value_size"

external cval_value_part : big_val_t -> int -> const_val_t
	= "camlidl_libasmir_cval_value_part"

external cval_ind : cval_t -> const_val_t
	= "camlidl_libasmir_cval_ind"

external cval_mem : cval_t -> bool
	= "camlidl_libasmir_cval_mem"

external cval_type : cval_t -> cval_type_t
	= "camlidl_libasmir_cval_type"

external cval_usage : cval_t -> int
	= "camlidl_libasmir_cval_usage"

external cval_taint : cval_t -> int
	= "camlidl_libasmir_cval_taint"

external call_has_lval : stmt -> bool
	= "camlidl_libasmir_call_has_lval"

external call_lval_opt : stmt -> exp
	= "camlidl_libasmir_call_lval_opt"

external call_fnname : stmt -> exp
	= "camlidl_libasmir_call_fnname"

external call_params : stmt -> exp array
	= "camlidl_libasmir_call_params"

external ret_has_exp : stmt -> bool
	= "camlidl_libasmir_ret_has_exp"

external ret_exp : stmt -> exp
	= "camlidl_libasmir_ret_exp"

external func_name : stmt -> string
	= "camlidl_libasmir_func_name"

external func_has_rv : stmt -> bool
	= "camlidl_libasmir_func_has_rv"

external func_rt : stmt -> reg_t
	= "camlidl_libasmir_func_rt"

external func_params : stmt -> stmt array
	= "camlidl_libasmir_func_params"

external func_is_external : stmt -> bool
	= "camlidl_libasmir_func_is_external"

external func_body : stmt -> stmt array
	= "camlidl_libasmir_func_body"

external assert_cond : stmt -> exp
	= "camlidl_libasmir_assert_cond"

external asmir_set_print_warning : bool -> unit
	= "camlidl_libasmir_asmir_set_print_warning"

external asmir_get_print_warning : unit -> bool
	= "camlidl_libasmir_asmir_get_print_warning"

external asmir_set_use_simple_segments : bool -> unit
	= "camlidl_libasmir_asmir_set_use_simple_segments"

external asmir_open_file : string -> asm_program_t
	= "camlidl_libasmir_asmir_open_file"

external asmir_close : asm_program_t -> unit
	= "camlidl_libasmir_asmir_close"

external asmir_string_of_insn : asm_program_t -> Libbfd.address_t -> string
	= "camlidl_libasmir_asmir_string_of_insn"

external asmir_get_asmp_arch : asm_program_t -> Libbfd.bfd_architecture
	= "camlidl_libasmir_asmir_get_asmp_arch"

external memory_cell_data_address : memory_cell_data_t -> Libbfd.address_t
	= "camlidl_libasmir_memory_cell_data_address"

external memory_cell_data_value : memory_cell_data_t -> int
	= "camlidl_libasmir_memory_cell_data_value"

external get_rodata : asm_program_t -> memory_data_t
	= "camlidl_libasmir_get_rodata"

external memory_data_size : memory_data_t -> int
	= "camlidl_libasmir_memory_data_size"

external memory_data_get : memory_data_t -> int -> memory_cell_data_t
	= "camlidl_libasmir_memory_data_get"

external destroy_memory_data : memory_data_t -> unit
	= "camlidl_libasmir_destroy_memory_data"

external asmir_bap_blocks_error : bap_blocks_t -> bool
	= "camlidl_libasmir_asmir_bap_blocks_error"

external asmir_bap_block_error : bap_block_t -> bool
	= "camlidl_libasmir_asmir_bap_block_error"

external asmir_addr_to_bap : asm_program_t -> Libbfd.address_t -> bap_block_t * Libbfd.address_t
	= "camlidl_libasmir_asmir_addr_to_bap"

external asmir_get_sec_startaddr : asm_program_t -> string -> Libbfd.address_t
	= "camlidl_libasmir_asmir_get_sec_startaddr"

external asmir_get_sec_endaddr : asm_program_t -> string -> Libbfd.address_t
	= "camlidl_libasmir_asmir_get_sec_endaddr"

external asmir_get_instr_length : asm_program_t -> Libbfd.address_t -> int
	= "camlidl_libasmir_asmir_get_instr_length"

external asmir_bap_blocks_size : bap_blocks_t -> int
	= "camlidl_libasmir_asmir_bap_blocks_size"

external asmir_bap_blocks_get : bap_blocks_t -> int -> bap_block_t
	= "camlidl_libasmir_asmir_bap_blocks_get"

external destroy_bap_block : bap_block_t -> unit
	= "camlidl_libasmir_destroy_bap_block"

external destroy_bap_blocks : bap_blocks_t -> unit
	= "camlidl_libasmir_destroy_bap_blocks"

external asmir_bap_block_size : bap_block_t -> int
	= "camlidl_libasmir_asmir_bap_block_size"

external asmir_bap_block_get : bap_block_t -> int -> stmt
	= "camlidl_libasmir_asmir_bap_block_get"

external asmir_bap_block_address : bap_block_t -> Libbfd.address_t
	= "camlidl_libasmir_asmir_bap_block_address"

external asm_string_from_stmt : stmt -> string
	= "camlidl_libasmir_asm_string_from_stmt"

external asm_string_from_block : bap_block_t -> string
	= "camlidl_libasmir_asm_string_from_block"

external asmir_bap_from_trace_file : string -> int64 -> int64 -> bool -> bool -> bap_blocks_t
	= "camlidl_libasmir_asmir_bap_from_trace_file"

external asmir_frames_from_trace_file : string -> int64 -> int64 -> trace_frames_t
	= "camlidl_libasmir_asmir_frames_from_trace_file"

external asmir_frames_length : trace_frames_t -> int
	= "camlidl_libasmir_asmir_frames_length"

external asmir_frames_get : trace_frames_t -> int -> trace_frame_t
	= "camlidl_libasmir_asmir_frames_get"

external asmir_frames_destroy : trace_frames_t -> unit
	= "camlidl_libasmir_asmir_frames_destroy"

external asmir_frame_type : trace_frame_t -> frame_type_t
	= "camlidl_libasmir_asmir_frame_type"

external asmir_frame_tid : trace_frame_t -> threadid_t
	= "camlidl_libasmir_asmir_frame_tid"

external asmir_frame_get_insn_bytes : trace_frame_t -> char array * Libbfd.address_t * int
	= "camlidl_libasmir_asmir_frame_get_insn_bytes"

external asmir_frame_get_loadmod_info : trace_frame_t -> string * Libbfd.address_t * Libbfd.address_t
	= "camlidl_libasmir_asmir_frame_get_loadmod_info"

external asmir_frame_get_syscall_info : trace_frame_t -> int * Libbfd.address_t * threadid_t
	= "camlidl_libasmir_asmir_frame_get_syscall_info"

external asmir_frame_get_except_info : trace_frame_t -> int * threadid_t * Libbfd.address_t * Libbfd.address_t
	= "camlidl_libasmir_asmir_frame_get_except_info"

external asmir_frame_get_operands : trace_frame_t -> cval_vec_t
	= "camlidl_libasmir_asmir_frame_get_operands"

external asmir_frame_destroy_operands : cval_vec_t -> unit
	= "camlidl_libasmir_asmir_frame_destroy_operands"

external asmir_frame_operands_length : cval_vec_t -> int
	= "camlidl_libasmir_asmir_frame_operands_length"

external asmir_frame_get_operand : cval_vec_t -> int -> cval_t
	= "camlidl_libasmir_asmir_frame_get_operand"

external asmir_get_bfd : asm_program_t -> Libbfd.bfdp
	= "camlidl_libasmir_asmir_get_bfd"

external byte_insn_to_asmp : Libbfd.bfd_architecture -> Libbfd.address_t -> char array -> asm_program_t
	= "camlidl_libasmir_byte_insn_to_asmp"

external asmir_free_vex_buffers : unit -> unit
	= "camlidl_libasmir_asmir_free_vex_buffers"

