/* File generated from libbfd.idl */

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


#include "libbfd.h"

void camlidl_ml2c_libbfd_address_t(value _v1, address_t * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int64_val(_v1);
}

value camlidl_c2ml_libbfd_address_t(address_t * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_int64((*_c2));
  return _v1;
}

int camlidl_transl_table_libbfd_enum_1[72] = {
  bfd_arch_unknown,
  bfd_arch_obscure,
  bfd_arch_m68k,
  bfd_arch_vax,
  bfd_arch_i960,
  bfd_arch_or32,
  bfd_arch_sparc,
  bfd_arch_spu,
  bfd_arch_mips,
  bfd_arch_i386,
  bfd_arch_we32k,
  bfd_arch_tahoe,
  bfd_arch_i860,
  bfd_arch_i370,
  bfd_arch_romp,
  bfd_arch_convex,
  bfd_arch_m88k,
  bfd_arch_m98k,
  bfd_arch_pyramid,
  bfd_arch_h8300,
  bfd_arch_pdp11,
  bfd_arch_powerpc,
  bfd_arch_rs6000,
  bfd_arch_hppa,
  bfd_arch_d10v,
  bfd_arch_d30v,
  bfd_arch_dlx,
  bfd_arch_m68hc11,
  bfd_arch_m68hc12,
  bfd_arch_z8k,
  bfd_arch_h8500,
  bfd_arch_sh,
  bfd_arch_alpha,
  bfd_arch_arm,
  bfd_arch_ns32k,
  bfd_arch_w65,
  bfd_arch_tic30,
  bfd_arch_tic4x,
  bfd_arch_tic54x,
  bfd_arch_tic80,
  bfd_arch_v850,
  bfd_arch_arc,
  bfd_arch_m32c,
  bfd_arch_m32r,
  bfd_arch_mn10200,
  bfd_arch_mn10300,
  bfd_arch_fr30,
  bfd_arch_frv,
  bfd_arch_mcore,
  bfd_arch_mep,
  bfd_arch_ia64,
  bfd_arch_ip2k,
  bfd_arch_iq2000,
  bfd_arch_mt,
  bfd_arch_pj,
  bfd_arch_avr,
  bfd_arch_bfin,
  bfd_arch_cr16,
  bfd_arch_cr16c,
  bfd_arch_crx,
  bfd_arch_cris,
  bfd_arch_s390,
  bfd_arch_score,
  bfd_arch_openrisc,
  bfd_arch_mmix,
  bfd_arch_xstormy16,
  bfd_arch_msp430,
  bfd_arch_xc16x,
  bfd_arch_xtensa,
  bfd_arch_maxq,
  bfd_arch_z80,
  bfd_arch_last,
};

int camlidl_ml2c_libbfd_enum_bfd_architecture(value _v1)
{
  int _c2;
  _c2 = camlidl_transl_table_libbfd_enum_1[Int_val(_v1)];
  return _c2;
}

value camlidl_c2ml_libbfd_enum_bfd_architecture(int _c1)
{
  value _v2;
  _v2 = camlidl_find_enum(_c1, camlidl_transl_table_libbfd_enum_1, 72, "enum bfd_architecture: bad enum bfd_architecture value");
  return _v2;
}

extern void camlidl_ml2c_libbfd_struct_bfd(value, struct bfd *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_struct_bfd(struct bfd *, camlidl_ctx _ctx);

void camlidl_ml2c_libbfd_bfdp(value _v1, bfdp * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((bfdp *) Bp_val(_v1));
}

value camlidl_c2ml_libbfd_bfdp(bfdp * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(bfdp) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((bfdp *) Bp_val(_v1)) = *_c2;
  return _v1;
}

extern void camlidl_ml2c_libbfd_struct_bfd_section(value, struct bfd_section *, camlidl_ctx _ctx);
extern value camlidl_c2ml_libbfd_struct_bfd_section(struct bfd_section *, camlidl_ctx _ctx);

void camlidl_ml2c_libbfd_section_ptr(value _v1, section_ptr * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((section_ptr *) Bp_val(_v1));
}

value camlidl_c2ml_libbfd_section_ptr(section_ptr * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(section_ptr) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((section_ptr *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_libbfd_struct_notreally(value _v1, struct notreally * _c2, camlidl_ctx _ctx)
{
  (*_c2).name = camlidl_malloc_string(_v1, _ctx);
}

value camlidl_c2ml_libbfd_struct_notreally(struct notreally * _c1, camlidl_ctx _ctx)
{
  value _v2;
  _v2 = copy_string((*_c1).name);
  return _v2;
}

void camlidl_ml2c_libbfd_struct_bfd_symbol(value _v1, struct bfd_symbol * _c2, camlidl_ctx _ctx)
{
  value _v3;
  value _v4;
  value _v5;
  value _v6;
  value _v7;
  _v3 = Field(_v1, 0);
  camlidl_ml2c_libbfd_bfdp(_v3, &(*_c2).the_bfd, _ctx);
  _v4 = Field(_v1, 1);
  (*_c2).name = camlidl_malloc_string(_v4, _ctx);
  _v5 = Field(_v1, 2);
  (*_c2).value = Int64_val(_v5);
  _v6 = Field(_v1, 3);
  (*_c2).flags = Int_val(_v6);
  _v7 = Field(_v1, 4);
  camlidl_ml2c_libbfd_section_ptr(_v7, &(*_c2).section, _ctx);
}

value camlidl_c2ml_libbfd_struct_bfd_symbol(struct bfd_symbol * _c1, camlidl_ctx _ctx)
{
  value _v2;
  value _v3[5];
  memset(_v3, 0, 5 * sizeof(value));
  Begin_roots_block(_v3, 5)
    _v3[0] = camlidl_c2ml_libbfd_bfdp(&(*_c1).the_bfd, _ctx);
    _v3[1] = copy_string((*_c1).name);
    _v3[2] = copy_int64((*_c1).value);
    _v3[3] = Val_int((*_c1).flags);
    _v3[4] = camlidl_c2ml_libbfd_section_ptr(&(*_c1).section, _ctx);
    _v2 = camlidl_alloc_small(5, 0);
    { mlsize_t _c4;
      for (_c4 = 0; _c4 < 5; _c4++) Field(_v2, _c4) = _v3[_c4];
    }
  End_roots()
  return _v2;
}

void camlidl_ml2c_libbfd_asymbol(value _v1, asymbol * _c2, camlidl_ctx _ctx)
{
  (*_c2) = (struct bfd_symbol  *) camlidl_malloc(sizeof(struct bfd_symbol ), _ctx);
  camlidl_ml2c_libbfd_struct_bfd_symbol(_v1, &*(*_c2), _ctx);
}

value camlidl_c2ml_libbfd_asymbol(asymbol * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_c2ml_libbfd_struct_bfd_symbol(&*(*_c2), _ctx);
  return _v1;
}

void camlidl_ml2c_libbfd_bfd_boolean(value _v1, bfd_boolean * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int_val(_v1);
}

value camlidl_c2ml_libbfd_bfd_boolean(bfd_boolean * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = Val_int((*_c2));
  return _v1;
}

void camlidl_ml2c_libbfd_file_ptr(value _v1, file_ptr * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int64_val(_v1);
}

value camlidl_c2ml_libbfd_file_ptr(file_ptr * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_int64((*_c2));
  return _v1;
}

void camlidl_ml2c_libbfd_bfd_size_type(value _v1, bfd_size_type * _c2, camlidl_ctx _ctx)
{
  (*_c2) = Int64_val(_v1);
}

value camlidl_c2ml_libbfd_bfd_size_type(bfd_size_type * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = copy_int64((*_c2));
  return _v1;
}

value camlidl_libbfd_bfd_get_section_contents(
	value _v_abfd,
	value _v_section,
	value _v_offset,
	value _v_count)
{
  bfdp abfd; /*in*/
  section_ptr section; /*in*/
  char *location; /*out*/
  file_ptr offset; /*in*/
  bfd_size_type count; /*in*/
  bfd_boolean _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_libbfd_bfdp(_v_abfd, &abfd, _ctx);
  camlidl_ml2c_libbfd_section_ptr(_v_section, &section, _ctx);
  camlidl_ml2c_libbfd_file_ptr(_v_offset, &offset, _ctx);
  camlidl_ml2c_libbfd_bfd_size_type(_v_count, &count, _ctx);
  location = stat_alloc(count * sizeof(char ));
  _res = bfd_get_section_contents(abfd, section, location, offset, count);
  Begin_roots_block(_vres, 2)
    _vres[0] = camlidl_c2ml_libbfd_bfd_boolean(&_res, _ctx);
    _vres[1] = alloc_bigarray_dims(
            BIGARRAY_UINT8 | BIGARRAY_C_LAYOUT | BIGARRAY_MANAGED,
            1, location, count);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_libbfd_bfd_section_get_vma(
	value _v_s)
{
  section_ptr s; /*in*/
  address_t _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libbfd_section_ptr(_v_s, &s, _ctx);
  _res = bfd_section_get_vma(s);
  _vres = camlidl_c2ml_libbfd_address_t(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libbfd_bfd_section_get_size(
	value _v_s)
{
  section_ptr s; /*in*/
  bfd_size_type _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libbfd_section_ptr(_v_s, &s, _ctx);
  _res = bfd_section_get_size(s);
  _vres = camlidl_c2ml_libbfd_bfd_size_type(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libbfd_bfd_section_get_name(
	value _v_s)
{
  section_ptr s; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libbfd_section_ptr(_v_s, &s, _ctx);
  _res = bfd_section_get_name(s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_libbfd_bfd_section_get_flags(
	value _v_s)
{
  section_ptr s; /*in*/
  long long _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_libbfd_section_ptr(_v_s, &s, _ctx);
  _res = bfd_section_get_flags(s);
  _vres = copy_int64(_res);
  camlidl_free(_ctx);
  return _vres;
}

