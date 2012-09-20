/* File generated from libasmir.idl */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include <caml/camlidlruntime.h>


#include "libasmir.h"

extern void camlidl_ml2c_libbfd_address_t(value, address_t *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_address_t(address_t *, camlidl_ctx _ctx);

extern int camlidl_ml2c_libbfd_enum_bfd_architecture(value);
extern value camlidl_c2ml_libbfd_enum_bfd_architecture(int);

extern int camlidl_transl_table_libbfd_enum_bfd_architecture[];

extern void camlidl_ml2c_libbfd_struct_bfd(value, struct bfd *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_struct_bfd(struct bfd *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_bfdp(value, bfdp *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_bfdp(bfdp *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_struct_bfd_section(value, struct bfd_section *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_struct_bfd_section(struct bfd_section *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_section_ptr(value, section_ptr *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_section_ptr(section_ptr *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_struct_notreally(value, struct notreally *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_struct_notreally(struct notreally *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_struct_bfd_symbol(value, struct bfd_symbol *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_struct_bfd_symbol(struct bfd_symbol *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_asymbol(value, asymbol *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_asymbol(asymbol *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_bfd_boolean(value, bfd_boolean *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_bfd_boolean(bfd_boolean *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_file_ptr(value, file_ptr *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_file_ptr(file_ptr *, camlidl_ctx _ctx);

extern void camlidl_ml2c_libbfd_bfd_size_type(value, bfd_size_type *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_bfd_size_type(bfd_size_type *, camlidl_ctx _ctx);

void camlidl_ml2c_libasmir_Exp(value _v1, Exp * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Exp *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_Exp(Exp * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Exp) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Exp *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_Stmt(value _v1, Stmt * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Stmt *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_Stmt(Stmt * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Stmt) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Stmt *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_asm_program_t(value _v1, asm_program_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((asm_program_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_asm_program_t(asm_program_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(asm_program_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((asm_program_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_bap_block_t(value _v1, bap_block_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((bap_block_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_bap_block_t(bap_block_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(bap_block_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((bap_block_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_bap_blocks_t(value _v1, bap_blocks_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((bap_blocks_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_bap_blocks_t(bap_blocks_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(bap_blocks_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((bap_blocks_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_cval_t(value _v1, cval_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((cval_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_cval_t(cval_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(cval_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((cval_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_trace_attrs(value _v1, trace_attrs * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((trace_attrs *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_trace_attrs(trace_attrs * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(trace_attrs) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((trace_attrs *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_cval_vec_t(value _v1, cval_vec_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((cval_vec_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_cval_vec_t(cval_vec_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(cval_vec_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((cval_vec_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_memory_cell_data_t(value _v1, memory_cell_data_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((memory_cell_data_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_memory_cell_data_t(memory_cell_data_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(memory_cell_data_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((memory_cell_data_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_memory_data_t(value _v1, memory_data_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((memory_data_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_memory_data_t(memory_data_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(memory_data_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((memory_data_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_trace_frames_t(value _v1, trace_frames_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((trace_frames_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_trace_frames_t(trace_frames_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(trace_frames_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((trace_frames_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_trace_frame_t(value _v1, trace_frame_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((trace_frame_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_trace_frame_t(trace_frame_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(trace_frame_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((trace_frame_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_big_val_t(value _v1, big_val_t * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((big_val_t *) Bp_val(_v1));
}

value camlidl_c2ml_libasmir_big_val_t(big_val_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(big_val_t) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((big_val_t *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libasmir_threadid_t(value _v1, threadid_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_libasmir_threadid_t(threadid_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((*_c2));
  return _v1;
}

     void null_check(void *ptr) {         if (ptr == NULL) {             caml_failwith("Unexpected NULL encountered.");         }     } 
value camlidl_libasmir_asmir_get_symbols(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  long *num; /*out*/
  asymbol *_res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  long _c1;
  mlsize_t _c2;
  value _v3;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  num = &_c1;
  _res = asmir_get_symbols(prog, num);
  Begin_roots_block(_vres, 2)
    _vres[0] = camlidl_alloc(*num, 0);
    Begin_root(_vres[0])
      for (_c2 = 0; _c2 < *num; _c2++) {
        _v3 = camlidl_c2ml_libbfd_asymbol(&_res[_c2], _ctx);
        modify(&Field(_vres[0], _c2), _v3);
      }
    End_roots()
    _vres[1] = Val_long(*num);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libasmir_asmir_get_all_symbols(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  long *num; /*out*/
  asymbol *_res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  long _c1;
  mlsize_t _c2;
  value _v3;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  num = &_c1;
  _res = asmir_get_all_symbols(prog, num);
  Begin_roots_block(_vres, 2)
    _vres[0] = camlidl_alloc(*num, 0);
    Begin_root(_vres[0])
      for (_c2 = 0; _c2 < *num; _c2++) {
        _v3 = camlidl_c2ml_libbfd_asymbol(&_res[_c2], _ctx);
        modify(&Field(_vres[0], _c2), _v3);
      }
    End_roots()
    _vres[1] = Val_long(*num);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libasmir_asmir_get_all_sections(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  long *num; /*out*/
  section_ptr *_res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  long _c1;
  mlsize_t _c2;
  value _v3;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  num = &_c1;
  _res = asmir_get_all_sections(prog, num);
  Begin_roots_block(_vres, 2)
    _vres[0] = camlidl_alloc(*num, 0);
    Begin_root(_vres[0])
      for (_c2 = 0; _c2 < *num; _c2++) {
        _v3 = camlidl_c2ml_libbfd_section_ptr(&_res[_c2], _ctx);
        modify(&Field(_vres[0], _c2), _v3);
      }
    End_roots()
    _vres[1] = Val_long(*num);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

int camlidl_transl_table_libasmir_enum_4[8] = {
  FRM_NONE,
  FRM_KEY,
  FRM_STD,
  FRM_LOADMOD,
  FRM_SYSCALL,
  FRM_TAINT,
  FRM_STD2,
  FRM_EXCEPT,
};

int camlidl_ml2c_libasmir_enum_frame_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_4[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_frame_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_4, 8, "enum frame_type_t: bad enum frame_type_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_5[11] = {
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

int camlidl_ml2c_libasmir_enum_exp_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_5[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_exp_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_5, 11, "enum exp_type_t: bad enum exp_type_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_6[5] = {
  REG_1,
  REG_8,
  REG_16,
  REG_32,
  REG_64,
};

int camlidl_ml2c_libasmir_enum_reg_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_6[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_reg_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_6, 5, "enum reg_t: bad enum reg_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_7[23] = {
  PLUS,
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

int camlidl_ml2c_libasmir_enum_binop_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_7[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_binop_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_7, 23, "enum binop_type_t: bad enum binop_type_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_8[2] = {
  NEG,
  NOT,
};

int camlidl_ml2c_libasmir_enum_unop_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_8[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_unop_type_t(int _c1)
{
  value _v2;
  switch(_c1) {
  case NEG: _v2 = Val_int(0); break;
  case NOT: _v2 = Val_int(1); break;
  default: invalid_argument("enum unop_type_t: bad enum unop_type_t value");
  }
  return _v2;
}

void camlidl_ml2c_libasmir_const_val_t(value _v1, const_val_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int64_val(_v1);
}

value camlidl_c2ml_libasmir_const_val_t(const_val_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_int64((*_c2));
  return _v1;
}

int camlidl_transl_table_libasmir_enum_9[8] = {
  CAST_UNSIGNED,
  CAST_SIGNED,
  CAST_HIGH,
  CAST_LOW,
  CAST_FLOAT,
  CAST_INTEGER,
  CAST_RFLOAT,
  CAST_RINTEGER,
};

int camlidl_ml2c_libasmir_enum_cast_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_9[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_cast_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_9, 8, "enum cast_t: bad enum cast_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_10[12] = {
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

int camlidl_ml2c_libasmir_enum_stmt_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_10[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_stmt_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_10, 12, "enum stmt_type_t: bad enum stmt_type_t value");
  return _v2;
}

int camlidl_transl_table_libasmir_enum_11[7] = {
  NONE,
  BOOL,
  CHR,
  INT_16,
  INT_32,
  INT_64,
  INT_128,
};

int camlidl_ml2c_libasmir_enum_cval_type_t(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libasmir_enum_11[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libasmir_enum_cval_type_t(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libasmir_enum_11, 7, "enum cval_type_t: bad enum cval_type_t value");
  return _v2;
}

value camlidl_libasmir_exp_type(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = exp_type(e);
  _vres = camlidl_c2ml_libasmir_enum_exp_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_binop_type(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = binop_type(e);
  _vres = camlidl_c2ml_libasmir_enum_binop_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_binop_lhs(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = binop_lhs(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_binop_rhs(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = binop_rhs(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_unop_type(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = unop_type(e);
  _vres = camlidl_c2ml_libasmir_enum_unop_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_unop_subexp(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = unop_subexp(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_mem_addr(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = mem_addr(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_mem_regtype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = mem_regtype(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_constant_val(
	value _v_e)
{
  Exp e; /*in*/
  const_val_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = constant_val(e);
  _vres = camlidl_c2ml_libasmir_const_val_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_constant_regtype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = constant_regtype(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_phi_phiname(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = phi_phiname(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_phi_numnodes(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = phi_numnodes(e);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_phi_nodeat(
	value _v_e,
	value _v_i)
{
  Exp e; /*in*/
  int i; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  i = Int_val(_v_i);
  _res = phi_nodeat(e, i);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_temp_regtype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = temp_regtype(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_temp_name(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = temp_name(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_unknown_str(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = unknown_str(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_unknown_regtype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = unknown_regtype(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cast_width(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = cast_width(e);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cast_casttype(
	value _v_e)
{
  Exp e; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = cast_casttype(e);
  _vres = camlidl_c2ml_libasmir_enum_cast_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cast_subexp(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = cast_subexp(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_name_string(
	value _v_e)
{
  Exp e; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = name_string(e);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_let_var(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = let_var(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_let_exp(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = let_exp(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_let_in(
	value _v_e)
{
  Exp e; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Exp(_v_e, &e, _ctx);
  _res = let_in(e);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_stmt_type(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = stmt_type(s);
  _vres = camlidl_c2ml_libasmir_enum_stmt_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_move_lhs(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = move_lhs(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_move_rhs(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = move_rhs(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_label_string(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = label_string(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_special_string(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = special_string(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_comment_string(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = comment_string(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_jmp_target(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = jmp_target(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cjmp_cond(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = cjmp_cond(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cjmp_ttarget(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = cjmp_ttarget(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cjmp_ftarget(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = cjmp_ftarget(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_expstmt_exp(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = expstmt_exp(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_vardecl_name(
	value _v_s)
{
  Stmt s; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = vardecl_name(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_vardecl_type(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = vardecl_type(s);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_stmt_attributes(
	value _v_s)
{
  Stmt s; /*in*/
  trace_attrs _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = stmt_attributes(s);
  _vres = camlidl_c2ml_libasmir_trace_attrs(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_trace_tid(
	value _v_ta)
{
  trace_attrs ta; /*in*/
  threadid_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_attrs(_v_ta, &ta, _ctx);
  _res = trace_tid(ta);
  _vres = camlidl_c2ml_libasmir_threadid_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_conc_map_size(
	value _v_ta)
{
  trace_attrs ta; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_attrs(_v_ta, &ta, _ctx);
  _res = conc_map_size(ta);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_get_cval(
	value _v_ta,
	value _v_i)
{
  trace_attrs ta; /*in*/
  int i; /*in*/
  cval_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_attrs(_v_ta, &ta, _ctx);
  i = Int_val(_v_i);
  _res = get_cval(ta, i);
  _vres = camlidl_c2ml_libasmir_cval_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_name(
	value _v_m)
{
  cval_t m; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_t(_v_m, &m, _ctx);
  _res = cval_name(m);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_value(
	value _v_m)
{
  cval_t m; /*in*/
  big_val_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_t(_v_m, &m, _ctx);
  _res = cval_value(m);
  _vres = camlidl_c2ml_libasmir_big_val_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_value_size(
	value _v_v)
{
  big_val_t v; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_big_val_t(_v_v, &v, _ctx);
  _res = cval_value_size(v);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_value_part(
	value _v_v,
	value _v_i)
{
  big_val_t v; /*in*/
  int i; /*in*/
  const_val_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_big_val_t(_v_v, &v, _ctx);
  i = Int_val(_v_i);
  _res = cval_value_part(v, i);
  _vres = camlidl_c2ml_libasmir_const_val_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_ind(
	value _v_m)
{
  cval_t m; /*in*/
  const_val_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_t(_v_m, &m, _ctx);
  _res = cval_ind(m);
  _vres = camlidl_c2ml_libasmir_const_val_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_mem(
	value _v_m)
{
  cval_t m; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_t(_v_m, &m, _ctx);
  _res = cval_mem(m);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_type(
	value _v_m)
{
  cval_t m; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_t(_v_m, &m, _ctx);
  _res = cval_type(m);
  _vres = camlidl_c2ml_libasmir_enum_cval_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_usage(
	value _v_m)
{
  cval_t m; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_t(_v_m, &m, _ctx);
  _res = cval_usage(m);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_cval_taint(
	value _v_m)
{
  cval_t m; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_t(_v_m, &m, _ctx);
  _res = cval_taint(m);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_has_lval(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_has_lval(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_lval_opt(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_lval_opt(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_fnname(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_fnname(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_call_params(
	value _v_s)
{
  Stmt s; /*in*/
  Exp *_res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = call_params(s);
  _c1 = camlidl_ptrarray_size((void **) _res);
  _vres = camlidl_alloc(_c1, 0);
  Begin_root(_vres)
    for (_c2 = 0; _c2 < _c1; _c2++) {
      _v3 = camlidl_c2ml_libasmir_Exp(&_res[_c2], _ctx);
      modify(&Field(_vres, _c2), _v3);
    }
  End_roots()
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_ret_has_exp(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = ret_has_exp(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_ret_exp(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = ret_exp(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_name(
	value _v_s)
{
  Stmt s; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_name(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_has_rv(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_has_rv(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_rt(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_rt(s);
  _vres = camlidl_c2ml_libasmir_enum_reg_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_params(
	value _v_s)
{
  Stmt s; /*in*/
  Stmt *_res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_params(s);
  _c1 = camlidl_ptrarray_size((void **) _res);
  _vres = camlidl_alloc(_c1, 0);
  Begin_root(_vres)
    for (_c2 = 0; _c2 < _c1; _c2++) {
      _v3 = camlidl_c2ml_libasmir_Stmt(&_res[_c2], _ctx);
      modify(&Field(_vres, _c2), _v3);
    }
  End_roots()
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_is_external(
	value _v_s)
{
  Stmt s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_is_external(s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_func_body(
	value _v_s)
{
  Stmt s; /*in*/
  Stmt *_res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = func_body(s);
  _c1 = camlidl_ptrarray_size((void **) _res);
  _vres = camlidl_alloc(_c1, 0);
  Begin_root(_vres)
    for (_c2 = 0; _c2 < _c1; _c2++) {
      _v3 = camlidl_c2ml_libasmir_Stmt(&_res[_c2], _ctx);
      modify(&Field(_vres, _c2), _v3);
    }
  End_roots()
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_assert_cond(
	value _v_s)
{
  Stmt s; /*in*/
  Exp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = assert_cond(s);
  _vres = camlidl_c2ml_libasmir_Exp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_set_print_warning(
	value _v_value)
{
  int value; /*in*/
  value = Int_val(_v_value);
  asmir_set_print_warning(value);
  return Val_unit;
}

value camlidl_libasmir_asmir_get_print_warning(value _unit)
{
  int _res;
  value _vres;

  _res = asmir_get_print_warning();
  _vres = Val_int(_res);
  return _vres;
}

value camlidl_libasmir_asmir_set_use_simple_segments(
	value _v_value)
{
  int value; /*in*/
  value = Int_val(_v_value);
  asmir_set_use_simple_segments(value);
  return Val_unit;
}

value camlidl_libasmir_asmir_open_file(
	value _v_filename)
{
  char *filename; /*in*/
  asm_program_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  _res = asmir_open_file(filename);
  null_check(_res);
  _vres = camlidl_c2ml_libasmir_asm_program_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_close(
	value _v_p)
{
  asm_program_t p; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  asmir_close(p);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_asmir_string_of_insn(
	value _v_prog,
	value _v_inst)
{
  asm_program_t prog; /*in*/
  address_t inst; /*in*/
  char *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  camlidl_ml2c_libbfd_address_t(_v_inst, &inst, _ctx);
  _res = asmir_string_of_insn(prog, inst);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_get_asmp_arch(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  _res = asmir_get_asmp_arch(prog);
  _vres = camlidl_c2ml_libbfd_enum_bfd_architecture(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_memory_cell_data_address(
	value _v_md)
{
  memory_cell_data_t md; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_cell_data_t(_v_md, &md, _ctx);
  _res = memory_cell_data_address(md);
  _vres = camlidl_c2ml_libbfd_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_memory_cell_data_value(
	value _v_md)
{
  memory_cell_data_t md; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_cell_data_t(_v_md, &md, _ctx);
  _res = memory_cell_data_value(md);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_get_rodata(
	value _v_prog)
{
  asm_program_t prog; /*in*/
  memory_data_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_prog, &prog, _ctx);
  _res = get_rodata(prog);
  _vres = camlidl_c2ml_libasmir_memory_data_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_memory_data_size(
	value _v_md)
{
  memory_data_t md; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_data_t(_v_md, &md, _ctx);
  _res = memory_data_size(md);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_memory_data_get(
	value _v_md,
	value _v_i)
{
  memory_data_t md; /*in*/
  int i; /*in*/
  memory_cell_data_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_data_t(_v_md, &md, _ctx);
  i = Int_val(_v_i);
  _res = memory_data_get(md, i);
  _vres = camlidl_c2ml_libasmir_memory_cell_data_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_destroy_memory_data(
	value _v_md)
{
  memory_data_t md; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_memory_data_t(_v_md, &md, _ctx);
  destroy_memory_data(md);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_asmir_bap_blocks_error(
	value _v_b)
{
  bap_blocks_t b; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_blocks_t(_v_b, &b, _ctx);
  _res = asmir_bap_blocks_error(b);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_bap_block_error(
	value _v_b)
{
  bap_block_t b; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_block_t(_v_b, &b, _ctx);
  _res = asmir_bap_block_error(b);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_addr_to_bap(
	value _v_p,
	value _v_addr)
{
  asm_program_t p; /*in*/
  address_t addr; /*in*/
  address_t *next; /*out*/
  bap_block_t _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  address_t _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  camlidl_ml2c_libbfd_address_t(_v_addr, &addr, _ctx);
  next = &_c1;
  _res = asmir_addr_to_bap(p, addr, next);
  Begin_roots_block(_vres, 2)
    _vres[0] = camlidl_c2ml_libasmir_bap_block_t(&_res, _ctx);
    _vres[1] = camlidl_c2ml_libbfd_address_t(&*next, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libasmir_asmir_get_sec_startaddr(
	value _v_p,
	value _v_sectionname)
{
  asm_program_t p; /*in*/
  char const *sectionname; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  sectionname = String_val(_v_sectionname);
  _res = asmir_get_sec_startaddr(p, sectionname);
  _vres = camlidl_c2ml_libbfd_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_get_sec_endaddr(
	value _v_p,
	value _v_sectionname)
{
  asm_program_t p; /*in*/
  char const *sectionname; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  sectionname = String_val(_v_sectionname);
  _res = asmir_get_sec_endaddr(p, sectionname);
  _vres = camlidl_c2ml_libbfd_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_get_instr_length(
	value _v_p,
	value _v_addr)
{
  asm_program_t p; /*in*/
  address_t addr; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  camlidl_ml2c_libbfd_address_t(_v_addr, &addr, _ctx);
  _res = asmir_get_instr_length(p, addr);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_bap_blocks_size(
	value _v_bs)
{
  bap_blocks_t bs; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_blocks_t(_v_bs, &bs, _ctx);
  _res = asmir_bap_blocks_size(bs);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_bap_blocks_get(
	value _v_bs,
	value _v_i)
{
  bap_blocks_t bs; /*in*/
  int i; /*in*/
  bap_block_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_blocks_t(_v_bs, &bs, _ctx);
  i = Int_val(_v_i);
  _res = asmir_bap_blocks_get(bs, i);
  _vres = camlidl_c2ml_libasmir_bap_block_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_destroy_bap_block(
	value _v_bs)
{
  bap_block_t bs; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_block_t(_v_bs, &bs, _ctx);
  destroy_bap_block(bs);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_destroy_bap_blocks(
	value _v_bs)
{
  bap_blocks_t bs; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_blocks_t(_v_bs, &bs, _ctx);
  destroy_bap_blocks(bs);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_asmir_bap_block_size(
	value _v_b)
{
  bap_block_t b; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_block_t(_v_b, &b, _ctx);
  _res = asmir_bap_block_size(b);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_bap_block_get(
	value _v_b,
	value _v_i)
{
  bap_block_t b; /*in*/
  int i; /*in*/
  Stmt _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_block_t(_v_b, &b, _ctx);
  i = Int_val(_v_i);
  _res = asmir_bap_block_get(b, i);
  _vres = camlidl_c2ml_libasmir_Stmt(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_bap_block_address(
	value _v_b)
{
  bap_block_t b; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_block_t(_v_b, &b, _ctx);
  _res = asmir_bap_block_address(b);
  _vres = camlidl_c2ml_libbfd_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asm_string_from_stmt(
	value _v_s)
{
  Stmt s; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_Stmt(_v_s, &s, _ctx);
  _res = asm_string_from_stmt(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asm_string_from_block(
	value _v_b)
{
  bap_block_t b; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_bap_block_t(_v_b, &b, _ctx);
  _res = asm_string_from_block(b);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_bap_from_trace_file(
	value _v_filename,
	value _v_offset,
	value _v_numisns,
	value _v_atts,
	value _v_pintrace)
{
  char *filename; /*in*/
  unsigned long long offset; /*in*/
  unsigned long long numisns; /*in*/
  int atts; /*in*/
  int pintrace; /*in*/
  bap_blocks_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  offset = Int64_val(_v_offset);
  numisns = Int64_val(_v_numisns);
  atts = Int_val(_v_atts);
  pintrace = Int_val(_v_pintrace);
  _res = asmir_bap_from_trace_file(filename, offset, numisns, atts, pintrace);
  _vres = camlidl_c2ml_libasmir_bap_blocks_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frames_from_trace_file(
	value _v_filename,
	value _v_offset,
	value _v_numisns)
{
  char *filename; /*in*/
  unsigned long long offset; /*in*/
  unsigned long long numisns; /*in*/
  trace_frames_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  filename = String_val(_v_filename);
  offset = Int64_val(_v_offset);
  numisns = Int64_val(_v_numisns);
  _res = asmir_frames_from_trace_file(filename, offset, numisns);
  _vres = camlidl_c2ml_libasmir_trace_frames_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frames_length(
	value _v_tfs)
{
  trace_frames_t tfs; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_frames_t(_v_tfs, &tfs, _ctx);
  _res = asmir_frames_length(tfs);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frames_get(
	value _v_tfs,
	value _v_index)
{
  trace_frames_t tfs; /*in*/
  int index; /*in*/
  trace_frame_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_frames_t(_v_tfs, &tfs, _ctx);
  index = Int_val(_v_index);
  _res = asmir_frames_get(tfs, index);
  _vres = camlidl_c2ml_libasmir_trace_frame_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frames_destroy(
	value _v_tfs)
{
  trace_frames_t tfs; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_frames_t(_v_tfs, &tfs, _ctx);
  asmir_frames_destroy(tfs);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_asmir_frame_type(
	value _v_tf)
{
  trace_frame_t tf; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_frame_t(_v_tf, &tf, _ctx);
  _res = asmir_frame_type(tf);
  _vres = camlidl_c2ml_libasmir_enum_frame_type_t(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frame_tid(
	value _v_tf)
{
  trace_frame_t tf; /*in*/
  threadid_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_frame_t(_v_tf, &tf, _ctx);
  _res = asmir_frame_tid(tf);
  _vres = camlidl_c2ml_libasmir_threadid_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frame_get_insn_bytes(
	value _v_tf)
{
  trace_frame_t tf; /*in*/
  address_t *addrout; /*out*/
  int *len; /*out*/
  char *_res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  address_t _c1;
  int _c2;
  mlsize_t _c3;
  value _v4;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  camlidl_ml2c_libasmir_trace_frame_t(_v_tf, &tf, _ctx);
  addrout = &_c1;
  len = &_c2;
  _res = asmir_frame_get_insn_bytes(tf, addrout, len);
  Begin_roots_block(_vres, 3)
    _vres[0] = camlidl_alloc(*len, 0);
    for (_c3 = 0; _c3 < *len; _c3++) {
      _v4 = Val_int((unsigned char)(_res[_c3]));
      modify(&Field(_vres[0], _c3), _v4);
    }
    _vres[1] = camlidl_c2ml_libbfd_address_t(&*addrout, _ctx);
    _vres[2] = Val_int(*len);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libasmir_asmir_frame_get_loadmod_info(
	value _v_tf)
{
  trace_frame_t tf; /*in*/
  address_t *lowout; /*out*/
  address_t *highout; /*out*/
  char const *_res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  address_t _c1;
  address_t _c2;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  camlidl_ml2c_libasmir_trace_frame_t(_v_tf, &tf, _ctx);
  lowout = &_c1;
  highout = &_c2;
  _res = asmir_frame_get_loadmod_info(tf, lowout, highout);
  Begin_roots_block(_vres, 3)
    _vres[0] = copy_string(_res);
    _vres[1] = camlidl_c2ml_libbfd_address_t(&*lowout, _ctx);
    _vres[2] = camlidl_c2ml_libbfd_address_t(&*highout, _ctx);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libasmir_asmir_frame_get_syscall_info(
	value _v_tf)
{
  trace_frame_t tf; /*in*/
  int *callno; /*out*/
  address_t *addr; /*out*/
  threadid_t *tid; /*out*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  address_t _c2;
  threadid_t _c3;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  camlidl_ml2c_libasmir_trace_frame_t(_v_tf, &tf, _ctx);
  callno = &_c1;
  addr = &_c2;
  tid = &_c3;
  asmir_frame_get_syscall_info(tf, callno, addr, tid);
  Begin_roots_block(_vres, 3)
    _vres[0] = Val_int(*callno);
    _vres[1] = camlidl_c2ml_libbfd_address_t(&*addr, _ctx);
    _vres[2] = camlidl_c2ml_libasmir_threadid_t(&*tid, _ctx);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libasmir_asmir_frame_get_except_info(
	value _v_tf)
{
  trace_frame_t tf; /*in*/
  int *exceptno; /*out*/
  threadid_t *tid; /*out*/
  address_t *from_addr; /*out*/
  address_t *to_addr; /*out*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  threadid_t _c2;
  address_t _c3;
  address_t _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  camlidl_ml2c_libasmir_trace_frame_t(_v_tf, &tf, _ctx);
  exceptno = &_c1;
  tid = &_c2;
  from_addr = &_c3;
  to_addr = &_c4;
  asmir_frame_get_except_info(tf, exceptno, tid, from_addr, to_addr);
  Begin_roots_block(_vres, 4)
    _vres[0] = Val_int(*exceptno);
    _vres[1] = camlidl_c2ml_libasmir_threadid_t(&*tid, _ctx);
    _vres[2] = camlidl_c2ml_libbfd_address_t(&*from_addr, _ctx);
    _vres[3] = camlidl_c2ml_libbfd_address_t(&*to_addr, _ctx);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libasmir_asmir_frame_get_operands(
	value _v_tf)
{
  trace_frame_t tf; /*in*/
  cval_vec_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_trace_frame_t(_v_tf, &tf, _ctx);
  _res = asmir_frame_get_operands(tf);
  _vres = camlidl_c2ml_libasmir_cval_vec_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frame_destroy_operands(
	value _v_cv)
{
  cval_vec_t cv; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_vec_t(_v_cv, &cv, _ctx);
  asmir_frame_destroy_operands(cv);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_libasmir_asmir_frame_operands_length(
	value _v_cv)
{
  cval_vec_t cv; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_vec_t(_v_cv, &cv, _ctx);
  _res = asmir_frame_operands_length(cv);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_frame_get_operand(
	value _v_cv,
	value _v_num)
{
  cval_vec_t cv; /*in*/
  int num; /*in*/
  cval_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_cval_vec_t(_v_cv, &cv, _ctx);
  num = Int_val(_v_num);
  _res = asmir_frame_get_operand(cv, num);
  _vres = camlidl_c2ml_libasmir_cval_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_get_bfd(
	value _v_p)
{
  asm_program_t p; /*in*/
  bfdp _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libasmir_asm_program_t(_v_p, &p, _ctx);
  _res = asmir_get_bfd(p);
  _vres = camlidl_c2ml_libbfd_bfdp(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_byte_insn_to_asmp(
	value _v_arch,
	value _v_addr,
	value _v_bb_bytes)
{
  int arch; /*in*/
  address_t addr; /*in*/
  char *bb_bytes; /*in*/
  int len; /*in*/
  asm_program_t _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  arch = camlidl_ml2c_libbfd_enum_bfd_architecture(_v_arch);
  camlidl_ml2c_libbfd_address_t(_v_addr, &addr, _ctx);
  _c1 = Wosize_val(_v_bb_bytes);
  bb_bytes = camlidl_malloc(_c1 * sizeof(char ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_bb_bytes, _c2);
    bb_bytes[_c2] = Int_val(_v3);
  }
  len = _c1;
  _res = byte_insn_to_asmp(arch, addr, bb_bytes, len);
  null_check(_res);
  _vres = camlidl_c2ml_libasmir_asm_program_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libasmir_asmir_free_vex_buffers(value _unit)
{
  asmir_free_vex_buffers();
  return Val_unit;
}

