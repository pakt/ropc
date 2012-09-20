/* Handling the mapping from instruction operands to registers */

#include <assert.h>
#include <stmt.h>
#include "reg_mapping.h"

static
string regid_to_name(uint32_t id)
{
  string name;
  switch(id) {
  case es_reg:
    name = string("ES");
    break;
  case cs_reg:
    name = string("CS");
    break;
  case ss_reg:
    name = string("SS");
    break;
  case ds_reg:
    name = string("DS");
    break;
  case fs_reg:
    name = string("FS");
    break;
  case gs_reg:
    name = string("GS");
    break;
  case al_reg:
    name = string("AL");
    break;
  case ah_reg:
    name = string("AH");
    break;
  case ax_reg:
    name = string("AX");
    break;
  case eax_reg:
    name = string("EAX");
    break;
  case cl_reg:
    name = string("CL");
    break;
  case ch_reg:
    name = string("CH");
    break;
  case cx_reg:
    name = string("CX");
    break;
  case ecx_reg:
    name = string("ECX");
    break;
  case dl_reg:
    name = string("DL");
    break;
  case dh_reg:
    name = string("DH");
    break;
  case dx_reg:
    name = string("DX");
    break;
  case edx_reg:
    name = string("EDX");
    break;
  case bl_reg:
    name = string("BL");
    break;
  case bh_reg:
    name = string("BH");
    break;
  case bx_reg:
    name = string("BX");
    break;
  case ebx_reg:
    name = string("EBX");
    break;
  case esp_reg:
    name = string("ESP");
    break;
  case sp_reg:
    name = string("SP");
    break;
  case ebp_reg:
    name = string("EBP");
    break;
  case bp_reg:
    name = string("BP");
    break;
  case esi_reg:
    name = string("ESI");
    break;
  case si_reg:
    name = string("SI");
    break;
  case edi_reg:
    name = string("EDI");
    break;
  case di_reg:
    name = string("DI");
    break;
  default:
    assert(0);
  }
  return name;
}

uint32_t regid_to_full(uint32_t id)
{
  uint32_t full;

  switch(id) {
  case es_reg:
  case cs_reg:
  case ss_reg:
  case ds_reg:
  case fs_reg:
  case gs_reg:
    full = id;
    break;
  case al_reg:
  case ah_reg:
  case ax_reg:
  case eax_reg:
    full = eax_reg;
    break;
  case cl_reg:
  case ch_reg:
  case cx_reg:
  case ecx_reg:
    full = ecx_reg;
    break;
  case dl_reg:
  case dh_reg:
  case dx_reg:
  case edx_reg:
    full = edx_reg;
    break;
  case bl_reg:
  case bh_reg:
  case bx_reg:
  case ebx_reg:
    full = ebx_reg;
    break;
  case esp_reg:
  case sp_reg:
    full = esp_reg;
    break;
  case ebp_reg:
  case bp_reg:
    full = ebp_reg;
    break;
  case esi_reg:
  case si_reg:
    full = esi_reg;
    break;
  case edi_reg:
  case di_reg:
    full = edi_reg;
    break;
  default:
    assert(0);
  }
  return full;
}

static uint32_t regid_to_write_mask(uint32_t id)
{
  uint32_t mask = 0;

  switch(id) {
  /* 8 bit low */
  case al_reg: 
  case cl_reg:
  case dl_reg:
  case bl_reg:
    mask = 0xFFFFFF00;
    break;
  /* 8 bit high */
  case ah_reg:
  case ch_reg:
  case dh_reg:
  case bh_reg:
    mask = 0xFFFF00FF;
    break;
  /* 16 bit */
  case ax_reg:
  case cx_reg:
  case dx_reg:
  case bx_reg:
  case sp_reg:
  case bp_reg:
  case si_reg:
  case di_reg:
  /* segment regs (also 16 bit) */
  case es_reg:
  case cs_reg:
  case ss_reg:
  case ds_reg:
  case fs_reg:
  case gs_reg:
    //    mask = 0xFFFF0000;
    mask = 0x0000;
    break;
  /* 32 bit */
  case eax_reg:
  case ecx_reg:
  case edx_reg:
  case ebx_reg:
  case esp_reg:
  case ebp_reg:
  case esi_reg:
  case edi_reg:
    mask = 0;
    break;
  default:
    assert(0);
  }
  return mask;
}

reg_t regid_to_type(uint32_t id)
{
  reg_t t = REG_1;

  switch(id) {
  /* 8 bit low */
  case al_reg: 
  case cl_reg:
  case dl_reg:
  case bl_reg:

  /* 8 bit high */
  case ah_reg:
  case ch_reg:
  case dh_reg:
  case bh_reg:
    t = REG_8;
    break;

  /* 16 bit */
  case ax_reg:
  case cx_reg:
  case dx_reg:
  case bx_reg:
  case sp_reg:
  case bp_reg:
  case si_reg:
  case di_reg:
  /* segment regs (also 16 bit) */
  case es_reg:
  case cs_reg:
  case ss_reg:
  case ds_reg:
  case fs_reg:
  case gs_reg:
    t = REG_16;
    break;
  /* 32 bit */
  case eax_reg:
  case ecx_reg:
  case edx_reg:
  case ebx_reg:
  case esp_reg:
  case ebp_reg:
  case esi_reg:
  case edi_reg:
    t = REG_32;
    break;
  default:
    assert(0);
  }
  return t;
}

static 
uint32_t regid_to_read_mask(uint32_t id)
{
  uint32_t mask = 0;

  switch(id) {
  /* special case for segment regs, which are 16 bit */
  case es_reg:
  case cs_reg:
  case ss_reg:
  case ds_reg:
  case fs_reg:
  case gs_reg:
    mask = 0xFFFF;
    break;
  default:
    mask = ~regid_to_write_mask(id);
  }

  return mask;
}

string register_name(uint32_t id)
{
  uint32_t fullreg = regid_to_full(id);
  string regname = regid_to_name(fullreg);
  return string("R_") + regname;
}

// generate a move instruction, assigning 'val' to register 'id'.
// the move is always into the full name of the register.
// i.e., an assignment to AX is translated to a modification of EAX
Move* write_reg(uint32_t id, Exp *val, int offset)
{
  uint32_t mask = regid_to_write_mask(id);
  uint32_t fullreg = regid_to_full(id);
  string regname = regid_to_name(fullreg);
  reg_t t = regid_to_type(fullreg);

  // set up lhs
  Temp *lhs = new Temp(t, string("R_") + regname);

  // set up value
  // needs to be shifted for "high bit" registers
  if(id == ah_reg || id == ch_reg || id == dh_reg || id == bh_reg) {
    if(val->exp_type == CONSTANT)
      ((Constant*)val)->val <<= 8;
    else
      val = new BinOp(LSHIFT,
		      val,
		      new Constant(REG_32,
				   8));
  }
  // make sure type is correct
  if(val->exp_type == CONSTANT)
    ((Constant*)val)->typ = t;
  else
    // make a narrowing cast. 
    // XXX: this is ugly since,
    // a) don't know if we need a cast at all
    // b) could need a widening cast
    // this should work for now. typechecker would help here, but
    // not available from c++
    // XXX NEW: should be widening?
    val = new Cast(val, t, CAST_UNSIGNED);

  if (offset >= 0) {
    // only writing to one byte.

    // drop old mask- make a new one based on writing one byte
    mask = ~(0xff << (offset * 8));
    if (id == ah_reg || id == ch_reg || id == dh_reg || id == bh_reg)
      mask <<= 8;

    // shift value
    if (offset != 0) {
      if(val->exp_type == CONSTANT)
	((Constant*)val)->val <<= (offset * 8);
      else
	val = new BinOp(LSHIFT,
			val,
			new Constant(REG_32,
				     offset * 8));
    }

  }

  if (mask != 0) {
    if (val->exp_type == CONSTANT) {
      ((Constant*)val)->val &= ~mask;
    } else {
      val = new BinOp(BITAND,
		      val,
		      new Constant(t,
				   ~mask));
    }
  }

  // set up rhs as:
  // reg & mask | val
  Exp *rhs = NULL;
  if (mask != 0) {
    Constant *mask_exp = new Constant(t,
				      mask);
    rhs = new BinOp(BITOR,
		    val,
		    new BinOp(BITAND,
			      mask_exp,
			      lhs->clone()));
  } else {
    rhs = val;
  }

  return new Move(lhs, rhs);
}

// always returns a 32 bit wide unsigned int
Exp* read_reg(uint32_t id)
{
  uint32_t fullreg = regid_to_full(id);
  string regname = regid_to_name(fullreg);  
  uint32_t mask = regid_to_read_mask(id);
  reg_t t = regid_to_type(fullreg);

  Exp *exp = new BinOp(BITAND,
		       new Temp(t, 
				string("R_") + regname),
		       new Constant(t,
				    mask));

  if(id == ah_reg || id == ch_reg || id == dh_reg || id == bh_reg) {
    exp = new BinOp(RSHIFT,
		    exp,
		    new Constant(REG_32,
				 8));
  }

  if(t != REG_32) {
    assert(t != REG_64);
    exp = new Cast(exp, REG_32, CAST_UNSIGNED);
  }

  return exp;
}

// returns the byte at the given offset of the given register,
// as a REG_8 Constant
Exp* read_reg(uint32_t id, int offset)
{
  Exp *exp = read_reg(id);
  uint32_t mask = 0xff;

  if (offset > 0)
    exp =  new BinOp(RSHIFT,
		     exp,
		     new Constant(REG_32,
				  offset * 8));

  exp = new BinOp(BITAND,
		  exp,
		  new Constant(REG_32,
			       mask));

  exp = new Cast(exp, REG_8, CAST_LOW);

  return exp;
}

string pin_register_name(uint32_t id)
{

switch (id) {
    //case REG_INVALID_ :
    //case REG_NONE :
    //case REG_FIRST:

    // immediate operand
    //case REG_IMM8 :
    //case REG_IMM_BASE:
    //case REG_IMM:
    //case REG_IMM32:
    //case REG_IMM_LAST:

    // memory operand
    //case REG_MEM:
    //case REG_MEM_BASE:
    //case REG_MEM_OFF8:
    //case REG_MEM_OFF32:
    //case REG_MEM_LAST:

    // memory-address offset operand
    //case REG_OFF8:
    //case REG_OFF_BASE:
    //case REG_OFF:
    //case REG_OFF32:
    //case REG_OFF_LAST:

    //case REG_MODX:

    // base for all kinds of registers (application: machine: pin)
    //case REG_RBASE: 

    // Machine registers are individual real registers on the machine
    //case REG_MACHINE_BASE:

    // Application registers are registers used in the application binary
    // Application registers include all machine registers. In addition:
    // they include some aggregrate registers that can be accessed by
    // the application in a single instruction
    // Essentially: application registers = individual machine registers + aggregrate registers
    
    //case REG_APPLICATION_BASE: 

    /* !@ todo: should save scratch mmx and fp registers */
    // The machine registers that form a context. These are the registers
    // that need to be saved in a context switch.
    //case REG_PHYSICAL_CONTEXT_BEGIN:
    
    //case REG_GR_BASE:
/*#if defined(TARGET_IA32E)
    // Context registers in the Intel(R) 64 architecture
    case REG_RDI = case REG_GR_BASE:  ///< rdi
    case REG_GDI = case REG_RDI:      ///< edi on a 32 bit machine: rdi on 64
    case REG_RSI:                ///< rsi
    case REG_GSI = case REG_RSI:      ///< esi on a 32 bit machine: rsi on 64
    case REG_RBP:                ///< rbp
    case REG_GBP = case REG_RBP:      ///< ebp on a 32 bit machine: rbp on 64
    case REG_RSP:                ///< rsp
    case REG_STACK_PTR = case REG_RSP:///< esp on a 32 bit machine: rsp on 64
    case REG_RBX:                ///< rbx
    case REG_GBX = case REG_RBX:      ///< ebx on a 32 bit machine: rbx on 64
    case REG_RDX:                ///< rdx
    case REG_GDX = case REG_RDX:      ///< edx on a 32 bit machine: rdx on 64
    case REG_RCX:                ///< rcx
    case REG_GCX = case REG_RCX:      ///< ecx on a 32 bit machine: rcx on 64
    case REG_RAX:                ///< rax
    case REG_GAX = case REG_RAX:      ///< eax on a 32 bit machine: rax on 64
    case REG_R8:
    case REG_R9:
    case REG_R10:
    case REG_R11:
    case REG_R12:
    case REG_R13:
    case REG_R14:
    case REG_R15:
    case REG_GR_LAST = case REG_R15:

    case REG_SEG_BASE:
    case REG_SEG_CS = case REG_SEG_BASE:
    case REG_SEG_SS:
    case REG_SEG_DS:
    case REG_SEG_ES:
    case REG_SEG_FS:
    case REG_SEG_GS:
    case REG_SEG_LAST = case REG_SEG_GS:

    case REG_RFLAGS:
    case REG_GFLAGS=case REG_RFLAGS:
    case REG_RIP:
    case REG_INST_PTR = case REG_RIP:
#else*/
    // Context registers in the IA-32 architecture
    case REG_EDI:  return string("R_EDI");
    //case REG_GDI:
    //case REG_EDI:
    case REG_ESI:  return string("R_ESI");
    //case REG_GSI:
    //case REG_ESI:
    case REG_EBP:  return string("R_EBP");
    //case REG_GBP:
    //case REG_EBP:
    case REG_ESP:  return string("R_ESP");
    //case REG_STACK_PTR:
    //case REG_ESP:
    case REG_EBX:  return string("R_EBX");
    //case REG_GBX:
    //case REG_EBX:
    case REG_EDX:  return string("R_EDX");
    //case REG_GDX:
    //case REG_EDX:
    case REG_ECX:  return string("R_ECX");
    //case REG_GCX:
    //case REG_ECX:
    case REG_EAX: return string("R_EAX");
    //case REG_GAX: 
    //case REG_EAX:
    //case REG_GR_LAST:
    //case REG_EAX:
    
    //case REG_SEG_BASE:
    //case REG_SEG_CS:
    //case REG_SEG_BASE:
    case REG_SEG_SS:  return string("SS");
    //case REG_SEG_DS:
    //case REG_SEG_ES:
    //case REG_SEG_FS:
    //case REG_SEG_GS:
    //case REG_SEG_LAST:
    //case REG_SEG_GS:

    case REG_EFLAGS:  return string("EFLAGS");
    //case REG_GFLAGS:
    //case REG_EFLAGS:
    //case REG_EIP:
    case REG_INST_PTR:  return string("R_EIP");
    //case REG_EIP:
//#endif
    
    //case REG_PHYSICAL_CONTEXT_END:
    //case REG_INST_PTR:

    // partial registers common to both the IA-32 and Intel(R) 64 architectures.
    case REG_AL:  return string("R_AL");
    case REG_AH:  return string("R_AH");
    case REG_AX:  return string("R_AX");
    
    case REG_CL:  return string("R_CL");
    case REG_CH:  return string("R_CH");
    case REG_CX:  return string("R_CX");
    
    case REG_DL:  return string("R_DL");
    case REG_DH:  return string("R_DH");
    case REG_DX:  return string("R_DX");
    
    case REG_BL:  return string("R_BL");
    case REG_BH:  
    case REG_BX:  return string("R_BX");

    case REG_BP:  return string("R_BP");
    case REG_SI:  return string("R_SI");
    case REG_DI:  return string("R_DI");

    //case REG_SP:
    //case REG_FLAGS:
    //case REG_IP:
    
/*#if defined(TARGET_IA32E)
    // partial registers in the Intel(R) 64 architecture
    case REG_EDI:
    case REG_DIL:
    case REG_ESI:
    case REG_SIL:
    case REG_EBP:
    case REG_BPL:
    case REG_ESP:
    case REG_SPL:
    case REG_EBX:
    case REG_EDX:
    case REG_ECX:
    case REG_EAX:
    case REG_EFLAGS:
    case REG_EIP:

    case REG_R8B:
    case REG_R8W:
    case REG_R8D:
    case REG_R9B:
    case REG_R9W:
    case REG_R9D:
    case REG_R10B:
    case REG_R10W:
    case REG_R10D:
    case REG_R11B:
    case REG_R11W:
    case REG_R11D:
    case REG_R12B:
    case REG_R12W:
    case REG_R12D:    
    case REG_R13B:
    case REG_R13W:
    case REG_R13D:
    case REG_R14B:
    case REG_R14W:
    case REG_R14D:
    case REG_R15B:
    case REG_R15W:
    case REG_R15D:    
#endif*/
    
/*
    case REG_MM_BASE:
    case REG_MM0 = case REG_MM_BASE:
    case REG_MM1:
    case REG_MM2:
    case REG_MM3:
    case REG_MM4:
    case REG_MM5:
    case REG_MM6:
    case REG_MM7:
    case REG_MM_LAST = case REG_MM7:
    
    case REG_EMM_BASE:
    case REG_EMM0 = case REG_EMM_BASE:
    case REG_EMM1:
    case REG_EMM2:
    case REG_EMM3:
    case REG_EMM4:
    case REG_EMM5:
    case REG_EMM6:
    case REG_EMM7:
    case REG_EMM_LAST = case REG_EMM7:

    case REG_MXT:
    
    case REG_XMM_BASE:
*/
    // case REG_XMM0 = case REG_XMM_BASE:
    case REG_XMM0:
      return string("XMM0");
    case REG_XMM1:
      return string("XMM1");
    case REG_XMM2:
      return string("XMM2");
    case REG_XMM3:
      return string("XMM3");
    case REG_XMM4:
      return string("XMM4");
    case REG_XMM5:
      return string("XMM5");
    case REG_XMM6:
      return string("XMM6");
    case REG_XMM7:
      return string("XMM7");
    
      //#if defined(TARGET_IA32E)
    // additional xmm registers in the Intel(R) 64 architecture
    // case REG_XMM8:
    //   return string("XMM8");
    // case REG_XMM9:
    //   return string("XMM9");
    // case REG_XMM10:
    //   return string("XMM10");
    // case REG_XMM11:
    //   return string("XMM11");
    // case REG_XMM12:
    //   return string("XMM12");
    // case REG_XMM13:
    //   return string("XMM13");
    // case REG_XMM14:
    //   return string("XMM14");
    // case REG_XMM15:
    //   return string("XMM15");
//     case REG_XMM_LAST = case REG_XMM15:
// #else    
//     case REG_XMM_LAST = case REG_XMM7:
// #endif
      /*
    case REG_YMM_BASE:
    case REG_YMM0 = case REG_YMM_BASE:
    case REG_YMM1:
    case REG_YMM2:
    case REG_YMM3:
    case REG_YMM4:
    case REG_YMM5:
    case REG_YMM6:
    case REG_YMM7:
    
#if defined(TARGET_IA32E)
    // additional ymm registers in the Intel(R) 64 architecture
    case REG_YMM8:
    case REG_YMM9:
    case REG_YMM10:
    case REG_YMM11:
    case REG_YMM12:
    case REG_YMM13:
    case REG_YMM14:
    case REG_YMM15:
    case REG_YMM_LAST = case REG_YMM15:
#else    
    case REG_YMM_LAST = case REG_YMM7:
#endif
    
    case REG_MXCSR:

    case REG_DR_BASE:
    case REG_DR0 = case REG_DR_BASE:
    case REG_DR1:
    case REG_DR2:
    case REG_DR3:
    case REG_DR4:
    case REG_DR5:
    case REG_DR6:
    case REG_DR7:
    case REG_DR_LAST = case REG_DR7:

    case REG_CR_BASE:
    case REG_CR0 = case REG_CR_BASE:
    case REG_CR1:
    case REG_CR2:
    case REG_CR3:
    case REG_CR4:
    case REG_CR_LAST = case REG_CR4:
    
    case REG_TSSR:
    
    case REG_LDTR:
*/
/*
 --- Not clear if following are needed
    case REG_ESR_BASE:
    case REG_ESR_LIMIT:
    
    case REG_CSR_BASE:
    case REG_CSR_LIMIT:
    
    case REG_SSR_BASE:
    case REG_SSR_LIMIT:
    
    case REG_DSR_BASE:
    case REG_DSR_LIMIT:
    
    case REG_FSR_BASE:
    case REG_FSR_LIMIT:
    
    case REG_GSR_BASE:
    case REG_GSR_LIMIT:
    
    case REG_TSSR_BASE:
    case REG_TSSR_LIMIT:
    
    case REG_LDTR_BASE:
    case REG_LDTR_LIMIT:
    
    case REG_GDTR_BASE:
    case REG_GDTR_LIMIT:
    
    case REG_IDTR_BASE:
    case REG_IDTR_LIMIT:
*/
/*
    case REG_TR_BASE:
    case REG_TR = case REG_TR_BASE:
    case REG_TR3:
    case REG_TR4:
    case REG_TR5:
    case REG_TR6:
    case REG_TR7:
    case REG_TR_LAST = case REG_TR7:
    
    case REG_FPST_BASE:
    case REG_FP_BASE = case REG_FPST_BASE:
    case REG_FPCW = case REG_FP_BASE:
    case REG_FPSW:
    case REG_FPTAG:
    case REG_FPIP_OFF:
    case REG_FPIP_SEL:
    case REG_FPOPCODE:
    case REG_FPDP_OFF:
    case REG_FPDP_SEL:
    case REG_FP_LAST = case REG_FPDP_SEL:
    
    case REG_ST_BASE:
    case REG_ST0 = case REG_ST_BASE:
    case REG_ST1:
    case REG_ST2:
    case REG_ST3:
    case REG_ST4:
    case REG_ST5:
    case REG_ST6:
    case REG_ST7:
#if !defined(TARGET_DOXYGEN)
    case REG_ST_LAST = case REG_ST7:
    case REG_FPST_LAST = case REG_ST_LAST:
    case REG_MACHINE_LAST = case REG_FPST_LAST:
    
    case REG_STATUS_FLAGS:
*/
    case REG_DF_FLAG: return string("R_DFLAG");
/*    
    case REG_AGGcase REGATE_BASE:
    case REG_FPST_ALL = case REG_AGGcase REGATE_BASE:
    case REG_AGGcase REGATE_LAST = case REG_FPST_ALL:

    case REG_APPLICATION_LAST = case REG_AGGcase REGATE_LAST: 
    
    case REG_PIN_BASE:
    case REG_PIN_GR_BASE = case REG_PIN_BASE:

    // ia32-specific Pin gr regs
    case REG_PIN_EDI = case REG_PIN_GR_BASE:
#if defined(TARGET_IA32)    
    case REG_PIN_GDI = case REG_PIN_EDI:                  // PIN_GDI == PIN_EDI on 32 bit: PIN_RDI on 64 bit.
#endif
    case REG_PIN_ESI:
    case REG_PIN_EBP:
    case REG_PIN_ESP:
#if defined (TARGET_IA32)
    case REG_PIN_STACK_PTR = case REG_PIN_ESP:
#endif    
    case REG_PIN_EBX:
    case REG_PIN_EDX:
#if defined(TARGET_IA32)    
    case REG_PIN_GDX = case REG_PIN_EDX:                  
#endif
    case REG_PIN_ECX:
#if defined(TARGET_IA32)    
    case REG_PIN_GCX = case REG_PIN_ECX:                  // PIN_GCX == PIN_ECX on 32 bit: PIN_RCX on 64 bit.
#endif
    case REG_PIN_EAX:
#if defined(TARGET_IA32)    
    case REG_PIN_GAX = case REG_PIN_EAX:                  // PIN_GAX == PIN_EAX on 32 bit: PIN_RAX on 64 bit.
#endif
    case REG_PIN_AL:
    case REG_PIN_AH:
    case REG_PIN_AX:
    case REG_PIN_CL:
    case REG_PIN_CH:
    case REG_PIN_CX:
    case REG_PIN_DL:
    case REG_PIN_DH:
    case REG_PIN_DX:
    case REG_PIN_BL:
    case REG_PIN_BH:
    case REG_PIN_BX:
    case REG_PIN_BP:
    case REG_PIN_SI:
    case REG_PIN_DI:
    case REG_PIN_SP:

#if defined(TARGET_IA32E)
    // Intel(R) 64 architecture specific pin gr regs
    case REG_PIN_RDI:
    case REG_PIN_GDI = case REG_PIN_RDI:
    case REG_PIN_RSI:
    case REG_PIN_RBP:
    case REG_PIN_RSP:
    
    case REG_PIN_STACK_PTR = case REG_PIN_RSP:
    
    case REG_PIN_RBX:
    case REG_PIN_RDX:
    case REG_PIN_GDX = case REG_PIN_RDX:
    case REG_PIN_RCX:
    case REG_PIN_GCX = case REG_PIN_RCX:
    case REG_PIN_RAX:
    case REG_PIN_GAX = case REG_PIN_RAX:
    case REG_PIN_R8:
    case REG_PIN_R9:
    case REG_PIN_R10:
    case REG_PIN_R11:
    case REG_PIN_R12:
    case REG_PIN_R13:
    case REG_PIN_R14:
    case REG_PIN_R15:

    case REG_PIN_DIL:
    case REG_PIN_SIL:
    case REG_PIN_BPL:
    case REG_PIN_SPL:
    
    case REG_PIN_R8B:
    case REG_PIN_R8W:
    case REG_PIN_R8D:

    case REG_PIN_R9B:
    case REG_PIN_R9W:
    case REG_PIN_R9D:

    case REG_PIN_R10B:
    case REG_PIN_R10W:
    case REG_PIN_R10D:

    case REG_PIN_R11B:
    case REG_PIN_R11W:
    case REG_PIN_R11D:

    case REG_PIN_R12B:
    case REG_PIN_R12W:
    case REG_PIN_R12D:

    case REG_PIN_R13B:
    case REG_PIN_R13W:
    case REG_PIN_R13D:

    case REG_PIN_R14B:
    case REG_PIN_R14W:
    case REG_PIN_R14D:

    case REG_PIN_R15B:
    case REG_PIN_R15W:
    case REG_PIN_R15D:
#endif

    // Every thread is assigned an index so we can implement tls
    case REG_THREAD_ID:
    
    case REG_SEG_GS_VAL:  // virtual reg holding actual value of gs
    case REG_SEG_FS_VAL:  // virtual reg holding actual value of fs

    // ISA-independent gr regs
    case REG_PIN_INDIRcase REG:  // virtual reg holding indirect jmp target value
    case REG_PIN_IPRELADDR: // virtual reg holding ip-rel address value
    case REG_PIN_SYSENTER_RESUMEADDR: // virtual reg holding the resume address from sysenter
    
    // ISA-independent gr regs holding temporary values
    case REG_PIN_T_BASE:
    case REG_PIN_T0 = case REG_PIN_T_BASE:        
    case REG_PIN_T1:        
    case REG_PIN_T2:
    case REG_PIN_T3:
    case REG_PIN_T0L:    // lower 8 bits of temporary register
    case REG_PIN_T1L:
    case REG_PIN_T2L:
    case REG_PIN_T3L:
    case REG_PIN_T0W:    // lower 16 bits of temporary register
    case REG_PIN_T1W:
    case REG_PIN_T2W:
    case REG_PIN_T3W:
    case REG_PIN_T0D:    // lower 32 bits of temporary register
    case REG_PIN_T1D:
    case REG_PIN_T2D:
    case REG_PIN_T3D:
    case REG_PIN_T_LAST = case REG_PIN_T3D:

#endif

    // Virtual registers reg holding memory addresses pointed by GS/FS registers
    // These registers are visible for tool writers
    */
    case REG_SEG_GS_BASE: return string("R_GS_BASE"); ///< Base address for GS segment
    case REG_SEG_FS_BASE: return string("R_FS_BASE");///< Base address for FS segment
      /*
    // ISA-independent Pin virtual regs needed for instrumentation
    // These are pin registers visible to the pintool writers.
    case REG_INST_BASE:
    case REG_INST_SCRATCH_BASE = case REG_INST_BASE:  ///< First available scratch register
    case REG_INST_G0 = case REG_INST_SCRATCH_BASE:    ///< Scratch register used in pintools
    case REG_INST_G1:                            ///< Scratch register used in pintools
    case REG_INST_G2:                            ///< Scratch register used in pintools
    case REG_INST_G3:                            ///< Scratch register used in pintools
    case REG_INST_G4:                            ///< Scratch register used in pintools
    case REG_INST_G5:                            ///< Scratch register used in pintools
    case REG_INST_G6:                            ///< Scratch register used in pintools
    case REG_INST_G7:                            ///< Scratch register used in pintools
    case REG_INST_G8:                            ///< Scratch register used in pintools
    case REG_INST_G9:                            ///< Scratch register used in pintools

    case REG_INST_TOOL_FIRST = case REG_INST_G0:     
    case REG_INST_TOOL_LAST = case REG_INST_G9:

    case REG_BUF_BASE0:
    case REG_BUF_BASE1:
    case REG_BUF_BASE2:
    case REG_BUF_BASE3:
    case REG_BUF_BASE4:
    case REG_BUF_BASE5:
    case REG_BUF_BASE6:
    case REG_BUF_BASE7:
    case REG_BUF_BASE8:
    case REG_BUF_BASE9:
    case REG_BUF_LAST = case REG_BUF_BASE9:

    case REG_BUF_END0:
    case REG_BUF_END1:
    case REG_BUF_END2:
    case REG_BUF_END3:
    case REG_BUF_END4:
    case REG_BUF_END5:
    case REG_BUF_END6:
    case REG_BUF_END7:
    case REG_BUF_END8:
    case REG_BUF_END9:
    case REG_BUF_ENDLAST = case REG_BUF_END9:

    case REG_INST_SCRATCH_LAST = case REG_BUF_ENDLAST:

#if !defined(TARGET_DOXYGEN)
    case REG_INST_COND:     // for conditional instrumentation.
    case REG_INST_LAST = case REG_INST_COND:
    
    // Used for memory rewriting: these are not live outside the region
    // but cannot use general purpose scratch registers: because they're
    // used during instrumentation generation: rather than region generation.
    case REG_INST_T0:
    case REG_INST_T0L:  
    case REG_INST_T0W: 
    case REG_INST_T0D:
    case REG_INST_T1:
    case REG_INST_T2:
    case REG_INST_T3:

    // Used to preserve the predicate value around repped string ops
    case REG_INST_PRESERVED_PREDICATE:

    // Used when the AC flag needs to be cleared before analysis routine
    case REG_FLAGS_BEFORE_AC_CLEARING:
    
    // Virtual regs used by Pin inside instrumentation bridges.
    // Unlike case REG_INST_BASE to case REG_INST_LAST: these registers are
    // NOT visible to  Pin clients.
    case REG_PIN_BRIDGE_ORIG_SP:    // hold the stack ptr value before the bridge
    case REG_PIN_BRIDGE_APP_IP: // hold the application (not code cache) IP to resume
    case REG_PIN_BRIDGE_SP_BEFORE_ALIGN: // hold the stack ptr value before the stack alignment
    case REG_PIN_BRIDGE_MARSHALLING_FRAME: // hold the address of the marshalled reference registers
    case REG_PIN_BRIDGE_CONTEXT_FRAME: // hold the address of the context frame
    case REG_PIN_BRIDGE_CONTEXT_ORIG_SP: // hold the sp at which the context was pushed

    case REG_PIN_SPILLPTR:  // ptr to the pin spill area
    case REG_PIN_GR_LAST = case REG_PIN_SPILLPTR:

    // case REG_PIN_FLAGS is x86-specific: but since it is not a gr: we put it out of
    // case REG_PIN_GR_BASE and case REG_PIN_GR_LAST

    case REG_PIN_STATUS_FLAGS:
    case REG_PIN_DF_FLAG:

    case REG_PIN_FLAGS:

    case REG_PIN_XMM_BASE:
    case REG_PIN_XMM0 = case REG_PIN_XMM_BASE:
    case REG_PIN_XMM1:
    case REG_PIN_XMM2:
    case REG_PIN_XMM3:
    case REG_PIN_XMM4:
    case REG_PIN_XMM5:
    case REG_PIN_XMM6:
    case REG_PIN_XMM7:
    case REG_PIN_XMM8:
    case REG_PIN_XMM9:
    case REG_PIN_XMM10:
    case REG_PIN_XMM11:
    case REG_PIN_XMM12:
    case REG_PIN_XMM13:
    case REG_PIN_XMM14:
    case REG_PIN_XMM15:

    case REG_PIN_YMM_BASE:
    case REG_PIN_YMM0 = case REG_PIN_YMM_BASE:
    case REG_PIN_YMM1:
    case REG_PIN_YMM2:
    case REG_PIN_YMM3:
    case REG_PIN_YMM4:
    case REG_PIN_YMM5:
    case REG_PIN_YMM6:
    case REG_PIN_YMM7:
    case REG_PIN_YMM8:
    case REG_PIN_YMM9:
    case REG_PIN_YMM10:
    case REG_PIN_YMM11:
    case REG_PIN_YMM12:
    case REG_PIN_YMM13:
    case REG_PIN_YMM14:
    case REG_PIN_YMM15:
    case REG_PIN_LAST = case REG_PIN_YMM15:
#endif
    case REG_LAST
*/
default: return "Unknown";

} 


}

cval_type_t get_type(uint32_t typ)
{
   switch (typ) {
   case VT_REG128:
   case VT_MEM128:
     return INT_128;
     break;

       case VT_REG64:
       case VT_MEM64:
         return INT_64;
         break;
       case VT_REG32: 
       case VT_MEM32: 
         return INT_32;
         break;
      case VT_REG16: 
      case VT_MEM16: 
         return INT_16;
         break;
      case VT_REG8: 
      case VT_MEM8: 
         return CHR;
         break;
      default:
        assert(false);
   }
   return NONE;
}
