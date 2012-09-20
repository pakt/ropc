/* File generated from libbfd.idl */

#ifndef _CAMLIDL_LIBBFD_H
#define _CAMLIDL_LIBBFD_H

#ifdef __cplusplus
#define _CAMLIDL_EXTERN_C extern "C"
#else
#define _CAMLIDL_EXTERN_C extern
#endif

#ifdef _WIN32
#pragma pack(push,8) /* necessary for COM interfaces */
#endif

typedef unsigned long long address_t;

enum bfd_architecture {
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

struct bfd;
typedef struct bfd *bfdp;

struct bfd_section;
typedef struct bfd_section *section_ptr;

struct notreally {
  char const *name;
};

struct bfd_symbol {
  bfdp the_bfd;
  char const *name;
  long long value;
  int flags;
  section_ptr section;
};

typedef struct bfd_symbol *asymbol;

typedef int bfd_boolean;

typedef long long file_ptr;

typedef unsigned long long bfd_size_type;

_CAMLIDL_EXTERN_C bfd_boolean bfd_get_section_contents(/*in*/ bfdp abfd, /*in*/ section_ptr section, /*out*/ char *location, /*in*/ file_ptr offset, /*in*/ bfd_size_type count);

_CAMLIDL_EXTERN_C address_t bfd_section_get_vma(/*in*/ section_ptr s);

_CAMLIDL_EXTERN_C bfd_size_type bfd_section_get_size(/*in*/ section_ptr s);

_CAMLIDL_EXTERN_C char const *bfd_section_get_name(/*in*/ section_ptr s);

_CAMLIDL_EXTERN_C long long bfd_section_get_flags(/*in*/ section_ptr s);

#define SEC_NO_FLAGS (0)

#define SEC_ALLOC (1)

#define SEC_LOAD (2)

#define SEC_RELOC (4)

#define SEC_READONLY (8)

#define SEC_CODE (16)

#define SEC_DATA (32)

#define SEC_ROM (64)

#define SEC_CONSTRUCTOR (128)

#define SEC_HAS_CONTENTS (256)

#define SEC_NEVER_LOAD (512)

#define SEC_THREAD_LOCAL (1024)

#define SEC_HAS_GOT_REF (2048)

#define SEC_IS_COMMON (4096)

#define SEC_DEBUGGING (8192)

#define SEC_IN_MEMORY (16384)

#define SEC_EXCLUDE (32768)

#define SEC_SORT_ENTRIES (65536)

#define SEC_LINK_ONCE (131072)

#define SEC_LINK_DUPLICATES (786432)

#define SEC_LINK_DUPLICATES_DISCARD (0)

#define SEC_LINK_DUPLICATES_ONE_ONLY (262144)

#define SEC_LINK_DUPLICATES_SAME_SIZE (524288)

#define SEC_LINK_DUPLICATES_SAME_CONTENTS ((262144LL | 524288LL))

#define SEC_LINKER_CREATED (1048576)

#define SEC_KEEP (2097152)

#define SEC_SMALL_DATA (4194304)

#define SEC_MERGE (8388608)

#define SEC_STRINGS (16777216)

#define SEC_GROUP (33554432)

#define SEC_COFF_SHARED_LIBRARY (67108864)

#define SEC_COFF_SHARED (134217728)

#define SEC_TIC54X_BLOCK (268435456)

#define SEC_TIC54X_CLINK (536870912)

#define SEC_COFF_NOREAD (1073741824)

#ifdef _WIN32
#pragma pack(pop)
#endif


#endif /* !_CAMLIDL_LIBBFD_H */
