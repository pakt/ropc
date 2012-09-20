(* File generated from libbfd.idl *)

type address_t = int64
and bfd_architecture =
  | Bfd_arch_unknown
  | Bfd_arch_obscure
  | Bfd_arch_m68k
  | Bfd_arch_vax
  | Bfd_arch_i960
  | Bfd_arch_or32
  | Bfd_arch_sparc
  | Bfd_arch_spu
  | Bfd_arch_mips
  | Bfd_arch_i386
  | Bfd_arch_we32k
  | Bfd_arch_tahoe
  | Bfd_arch_i860
  | Bfd_arch_i370
  | Bfd_arch_romp
  | Bfd_arch_convex
  | Bfd_arch_m88k
  | Bfd_arch_m98k
  | Bfd_arch_pyramid
  | Bfd_arch_h8300
  | Bfd_arch_pdp11
  | Bfd_arch_powerpc
  | Bfd_arch_rs6000
  | Bfd_arch_hppa
  | Bfd_arch_d10v
  | Bfd_arch_d30v
  | Bfd_arch_dlx
  | Bfd_arch_m68hc11
  | Bfd_arch_m68hc12
  | Bfd_arch_z8k
  | Bfd_arch_h8500
  | Bfd_arch_sh
  | Bfd_arch_alpha
  | Bfd_arch_arm
  | Bfd_arch_ns32k
  | Bfd_arch_w65
  | Bfd_arch_tic30
  | Bfd_arch_tic4x
  | Bfd_arch_tic54x
  | Bfd_arch_tic80
  | Bfd_arch_v850
  | Bfd_arch_arc
  | Bfd_arch_m32c
  | Bfd_arch_m32r
  | Bfd_arch_mn10200
  | Bfd_arch_mn10300
  | Bfd_arch_fr30
  | Bfd_arch_frv
  | Bfd_arch_mcore
  | Bfd_arch_mep
  | Bfd_arch_ia64
  | Bfd_arch_ip2k
  | Bfd_arch_iq2000
  | Bfd_arch_mt
  | Bfd_arch_pj
  | Bfd_arch_avr
  | Bfd_arch_bfin
  | Bfd_arch_cr16
  | Bfd_arch_cr16c
  | Bfd_arch_crx
  | Bfd_arch_cris
  | Bfd_arch_s390
  | Bfd_arch_score
  | Bfd_arch_openrisc
  | Bfd_arch_mmix
  | Bfd_arch_xstormy16
  | Bfd_arch_msp430
  | Bfd_arch_xc16x
  | Bfd_arch_xtensa
  | Bfd_arch_maxq
  | Bfd_arch_z80
  | Bfd_arch_last
and bfdp
and section_ptr
and notreally = string
and bfd_symbol = {
  bfd_symbol_the_bfd: bfdp;
  bfd_symbol_name: string;
  bfd_symbol_value: int64;
  bfd_symbol_flags: int;
  bfd_symbol_section: section_ptr;
}
and asymbol = bfd_symbol
and bfd_boolean = int
and file_ptr = int64
and bfd_size_type = int64

external bfd_get_section_contents : bfdp -> section_ptr -> file_ptr -> bfd_size_type -> bfd_boolean * (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
	= "camlidl_libbfd_bfd_get_section_contents"

external bfd_section_get_vma : section_ptr -> address_t
	= "camlidl_libbfd_bfd_section_get_vma"

external bfd_section_get_size : section_ptr -> bfd_size_type
	= "camlidl_libbfd_bfd_section_get_size"

external bfd_section_get_name : section_ptr -> string
	= "camlidl_libbfd_bfd_section_get_name"

external bfd_section_get_flags : section_ptr -> int64
	= "camlidl_libbfd_bfd_section_get_flags"

let sEC_NO_FLAGS = Int64.of_string "0"

let sEC_ALLOC = Int64.of_string "1"

let sEC_LOAD = Int64.of_string "2"

let sEC_RELOC = Int64.of_string "4"

let sEC_READONLY = Int64.of_string "8"

let sEC_CODE = Int64.of_string "16"

let sEC_DATA = Int64.of_string "32"

let sEC_ROM = Int64.of_string "64"

let sEC_CONSTRUCTOR = Int64.of_string "128"

let sEC_HAS_CONTENTS = Int64.of_string "256"

let sEC_NEVER_LOAD = Int64.of_string "512"

let sEC_THREAD_LOCAL = Int64.of_string "1024"

let sEC_HAS_GOT_REF = Int64.of_string "2048"

let sEC_IS_COMMON = Int64.of_string "4096"

let sEC_DEBUGGING = Int64.of_string "8192"

let sEC_IN_MEMORY = Int64.of_string "16384"

let sEC_EXCLUDE = Int64.of_string "32768"

let sEC_SORT_ENTRIES = Int64.of_string "65536"

let sEC_LINK_ONCE = Int64.of_string "131072"

let sEC_LINK_DUPLICATES = Int64.of_string "786432"

let sEC_LINK_DUPLICATES_DISCARD = Int64.of_string "0"

let sEC_LINK_DUPLICATES_ONE_ONLY = Int64.of_string "262144"

let sEC_LINK_DUPLICATES_SAME_SIZE = Int64.of_string "524288"

let sEC_LINK_DUPLICATES_SAME_CONTENTS = Int64.of_string "786432"

let sEC_LINKER_CREATED = Int64.of_string "1048576"

let sEC_KEEP = Int64.of_string "2097152"

let sEC_SMALL_DATA = Int64.of_string "4194304"

let sEC_MERGE = Int64.of_string "8388608"

let sEC_STRINGS = Int64.of_string "16777216"

let sEC_GROUP = Int64.of_string "33554432"

let sEC_COFF_SHARED_LIBRARY = Int64.of_string "67108864"

let sEC_COFF_SHARED = Int64.of_string "134217728"

let sEC_TIC54X_BLOCK = Int64.of_string "268435456"

let sEC_TIC54X_CLINK = Int64.of_string "536870912"

let sEC_COFF_NOREAD = Int64.of_string "1073741824"

