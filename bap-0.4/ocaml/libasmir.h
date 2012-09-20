/* File generated from libasmir.idl */

#ifndef _CAMLIDL_LIBASMIR_H
#define _CAMLIDL_LIBASMIR_H

#ifdef __cplusplus
#define _CAMLIDL_EXTERN_C extern "C"
#else
#define _CAMLIDL_EXTERN_C extern
#endif

#ifdef _WIN32
#pragma pack(push,8) /* necessary for COM interfaces */
#endif

#include "libbfd.h"
typedef void *Exp;

typedef void *Stmt;

typedef void *asm_program_t;

typedef void *bap_block_t;

typedef void *bap_blocks_t;

typedef void *cval_t;

typedef void *trace_attrs;

typedef void *cval_vec_t;

typedef void *memory_cell_data_t;

typedef void *memory_data_t;

typedef void *trace_frames_t;

typedef void *trace_frame_t;

typedef void *big_val_t;

typedef int threadid_t;

_CAMLIDL_EXTERN_C asymbol *asmir_get_symbols(/*in*/ asm_program_t prog, /*out*/ long *num);

_CAMLIDL_EXTERN_C asymbol *asmir_get_all_symbols(/*in*/ asm_program_t prog, /*out*/ long *num);

_CAMLIDL_EXTERN_C section_ptr *asmir_get_all_sections(/*in*/ asm_program_t prog, /*out*/ long *num);

enum frame_type_t {
FRM_NONE = 0,
FRM_KEY,
FRM_STD,
FRM_LOADMOD,
FRM_SYSCALL,
FRM_TAINT,
FRM_STD2,
FRM_EXCEPT,
};

enum exp_type_t {
BINOP,
UNOP,
CONSTANT,
MEM,
TEMP,
PHI,
CAST,
NAME,
UNKNOWN,
LET,
EXTENSION,
};

enum reg_t {
REG_1,
REG_8,
REG_16,
REG_32,
REG_64,
};

enum binop_type_t {
PLUS = 0,
MINUS,
TIMES,
DIVIDE,
MOD,
LSHIFT,
RSHIFT,
ARSHIFT,
LROTATE,
RROTATE,
LOGICAND,
LOGICOR,
BITAND,
BITOR,
XOR,
EQ,
NEQ,
GT,
LT,
GE,
LE,
SDIVIDE,
SMOD,
};

enum unop_type_t {
NEG,
NOT,
};

typedef long long const_val_t;

enum cast_t {
CAST_UNSIGNED,
CAST_SIGNED,
CAST_HIGH,
CAST_LOW,
CAST_FLOAT,
CAST_INTEGER,
CAST_RFLOAT,
CAST_RINTEGER,
};

enum stmt_type_t {
JMP,
CJMP,
SPECIAL,
MOVE,
COMMENT,
LABEL,
EXPSTMT,
VARDECL,
CALL,
RETURN,
FUNCTION,
ASSERT,
};

enum cval_type_t {
NONE,
BOOL,
CHR,
INT_16,
INT_32,
INT_64,
INT_128,
};

_CAMLIDL_EXTERN_C int exp_type(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int binop_type(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp binop_lhs(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp binop_rhs(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int unop_type(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp unop_subexp(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp mem_addr(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int mem_regtype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C const_val_t constant_val(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int constant_regtype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *phi_phiname(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int phi_numnodes(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp phi_nodeat(/*in*/ Exp e, /*in*/ int i);

_CAMLIDL_EXTERN_C int temp_regtype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *temp_name(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *unknown_str(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int unknown_regtype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int cast_width(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int cast_casttype(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp cast_subexp(/*in*/ Exp e);

_CAMLIDL_EXTERN_C char *name_string(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp let_var(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp let_exp(/*in*/ Exp e);

_CAMLIDL_EXTERN_C Exp let_in(/*in*/ Exp e);

_CAMLIDL_EXTERN_C int stmt_type(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp move_lhs(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp move_rhs(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *label_string(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *special_string(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *comment_string(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp jmp_target(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp cjmp_cond(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp cjmp_ttarget(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp cjmp_ftarget(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp expstmt_exp(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char *vardecl_name(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int vardecl_type(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C trace_attrs stmt_attributes(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C threadid_t trace_tid(/*in*/ trace_attrs ta);

_CAMLIDL_EXTERN_C int conc_map_size(/*in*/ trace_attrs ta);

_CAMLIDL_EXTERN_C cval_t get_cval(/*in*/ trace_attrs ta, /*in*/ int i);

_CAMLIDL_EXTERN_C char *cval_name(/*in*/ cval_t m);

_CAMLIDL_EXTERN_C big_val_t cval_value(/*in*/ cval_t m);

_CAMLIDL_EXTERN_C int cval_value_size(/*in*/ big_val_t v);

_CAMLIDL_EXTERN_C const_val_t cval_value_part(/*in*/ big_val_t v, /*in*/ int i);

_CAMLIDL_EXTERN_C const_val_t cval_ind(/*in*/ cval_t m);

_CAMLIDL_EXTERN_C int cval_mem(/*in*/ cval_t m);

_CAMLIDL_EXTERN_C int cval_type(/*in*/ cval_t m);

_CAMLIDL_EXTERN_C int cval_usage(/*in*/ cval_t m);

_CAMLIDL_EXTERN_C int cval_taint(/*in*/ cval_t m);

_CAMLIDL_EXTERN_C int call_has_lval(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp call_lval_opt(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp call_fnname(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp *call_params(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int ret_has_exp(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp ret_exp(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char const *func_name(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int func_has_rv(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int func_rt(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Stmt *func_params(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C int func_is_external(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Stmt *func_body(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C Exp assert_cond(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C void asmir_set_print_warning(/*in*/ int value);

_CAMLIDL_EXTERN_C int asmir_get_print_warning(void);

_CAMLIDL_EXTERN_C void asmir_set_use_simple_segments(/*in*/ int value);

_CAMLIDL_EXTERN_C asm_program_t asmir_open_file(/*in*/ char *filename);

_CAMLIDL_EXTERN_C void asmir_close(/*in*/ asm_program_t p);

_CAMLIDL_EXTERN_C char *asmir_string_of_insn(/*in*/ asm_program_t prog, /*in*/ address_t inst);

_CAMLIDL_EXTERN_C int asmir_get_asmp_arch(/*in*/ asm_program_t prog);

_CAMLIDL_EXTERN_C address_t memory_cell_data_address(/*in*/ memory_cell_data_t md);

_CAMLIDL_EXTERN_C int memory_cell_data_value(/*in*/ memory_cell_data_t md);

_CAMLIDL_EXTERN_C memory_data_t get_rodata(/*in*/ asm_program_t prog);

_CAMLIDL_EXTERN_C int memory_data_size(/*in*/ memory_data_t md);

_CAMLIDL_EXTERN_C memory_cell_data_t memory_data_get(/*in*/ memory_data_t md, /*in*/ int i);

_CAMLIDL_EXTERN_C void destroy_memory_data(/*in*/ memory_data_t md);

_CAMLIDL_EXTERN_C int asmir_bap_blocks_error(/*in*/ bap_blocks_t b);

_CAMLIDL_EXTERN_C int asmir_bap_block_error(/*in*/ bap_block_t b);

_CAMLIDL_EXTERN_C bap_block_t asmir_addr_to_bap(/*in*/ asm_program_t p, /*in*/ address_t addr, /*out*/ address_t *next);

_CAMLIDL_EXTERN_C address_t asmir_get_sec_startaddr(/*in*/ asm_program_t p, /*in*/ char const *sectionname);

_CAMLIDL_EXTERN_C address_t asmir_get_sec_endaddr(/*in*/ asm_program_t p, /*in*/ char const *sectionname);

_CAMLIDL_EXTERN_C int asmir_get_instr_length(/*in*/ asm_program_t p, /*in*/ address_t addr);

_CAMLIDL_EXTERN_C int asmir_bap_blocks_size(/*in*/ bap_blocks_t bs);

_CAMLIDL_EXTERN_C bap_block_t asmir_bap_blocks_get(/*in*/ bap_blocks_t bs, /*in*/ int i);

_CAMLIDL_EXTERN_C void destroy_bap_block(/*in*/ bap_block_t bs);

_CAMLIDL_EXTERN_C void destroy_bap_blocks(/*in*/ bap_blocks_t bs);

_CAMLIDL_EXTERN_C int asmir_bap_block_size(/*in*/ bap_block_t b);

_CAMLIDL_EXTERN_C Stmt asmir_bap_block_get(/*in*/ bap_block_t b, /*in*/ int i);

_CAMLIDL_EXTERN_C address_t asmir_bap_block_address(/*in*/ bap_block_t b);

_CAMLIDL_EXTERN_C char const *asm_string_from_stmt(/*in*/ Stmt s);

_CAMLIDL_EXTERN_C char const *asm_string_from_block(/*in*/ bap_block_t b);

_CAMLIDL_EXTERN_C bap_blocks_t asmir_bap_from_trace_file(/*in*/ char *filename, /*in*/ unsigned long long offset, /*in*/ unsigned long long numisns, /*in*/ int atts, /*in*/ int pintrace);

_CAMLIDL_EXTERN_C trace_frames_t asmir_frames_from_trace_file(/*in*/ char *filename, /*in*/ unsigned long long offset, /*in*/ unsigned long long numisns);

_CAMLIDL_EXTERN_C int asmir_frames_length(/*in*/ trace_frames_t tfs);

_CAMLIDL_EXTERN_C trace_frame_t asmir_frames_get(/*in*/ trace_frames_t tfs, /*in*/ int index);

_CAMLIDL_EXTERN_C void asmir_frames_destroy(/*in*/ trace_frames_t tfs);

_CAMLIDL_EXTERN_C int asmir_frame_type(/*in*/ trace_frame_t tf);

_CAMLIDL_EXTERN_C threadid_t asmir_frame_tid(/*in*/ trace_frame_t tf);

_CAMLIDL_EXTERN_C char *asmir_frame_get_insn_bytes(/*in*/ trace_frame_t tf, /*out*/ address_t *addrout, /*out*/ int *len);

_CAMLIDL_EXTERN_C char const *asmir_frame_get_loadmod_info(/*in*/ trace_frame_t tf, /*out*/ address_t *lowout, /*out*/ address_t *highout);

_CAMLIDL_EXTERN_C void asmir_frame_get_syscall_info(/*in*/ trace_frame_t tf, /*out*/ int *callno, /*out*/ address_t *addr, /*out*/ threadid_t *tid);

_CAMLIDL_EXTERN_C void asmir_frame_get_except_info(/*in*/ trace_frame_t tf, /*out*/ int *exceptno, /*out*/ threadid_t *tid, /*out*/ address_t *from_addr, /*out*/ address_t *to_addr);

_CAMLIDL_EXTERN_C cval_vec_t asmir_frame_get_operands(/*in*/ trace_frame_t tf);

_CAMLIDL_EXTERN_C void asmir_frame_destroy_operands(/*in*/ cval_vec_t cv);

_CAMLIDL_EXTERN_C int asmir_frame_operands_length(/*in*/ cval_vec_t cv);

_CAMLIDL_EXTERN_C cval_t asmir_frame_get_operand(/*in*/ cval_vec_t cv, /*in*/ int num);

_CAMLIDL_EXTERN_C bfdp asmir_get_bfd(/*in*/ asm_program_t p);

_CAMLIDL_EXTERN_C asm_program_t byte_insn_to_asmp(/*in*/ int arch, /*in*/ address_t addr, /*in*/ char *bb_bytes, /*in*/ int len);

_CAMLIDL_EXTERN_C void asmir_free_vex_buffers(void);

#ifdef _WIN32
#pragma pack(pop)
#endif


#endif /* !_CAMLIDL_LIBASMIR_H */
