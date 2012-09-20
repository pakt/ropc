// -*- c++ -*-

#pragma once

#include <stdint.h>
#include <string.h>

#include "pin.H"

/** An emulated 8-bit register */
typedef unsigned char reg8;

/** An emulated 16-bit register. */
union reg16 {
   reg8 parts[2];
   unsigned short full;
};

/** An emulated 32-bit register. */
union reg32 {
   reg16 parts[2];
   unsigned int full;
};

struct RegCache {

   reg32 eax;
   reg32 ebx;
   reg32 ecx;
   reg32 edx;
   reg32 esi;
   reg32 edi;
   reg32 esp;
   reg32 ebp;
   reg32 eflags;

   reg16 cs;
   reg16 ds;
   reg16 ss;
   reg16 es;
   reg16 fs;
   reg16 gs;

   // Catch-all for all other registers.
   reg32 catchall;

   // Mapping between register value holders and Pin register
   // names. Provides a way to resolve from a Pin register name into a
   // reference to the associated cache value.
private:
   reg8 *_mapping[REG_LAST];

public:

   RegCache()
   {
      for(int i = 0; i < REG_LAST; i++)
         _mapping[i] = (reg8 *) &catchall;

      _mapping[REG_EAX] = (reg8 *) &eax;
      _mapping[REG_EBX] = (reg8 *) &ebx;
      _mapping[REG_ECX] = (reg8 *) &ecx;
      _mapping[REG_EDX] = (reg8 *) &edx;
      _mapping[REG_ESI] = (reg8 *) &esi;
      _mapping[REG_EDI] = (reg8 *) &edi;
      _mapping[REG_ESP] = (reg8 *) &esp;
      _mapping[REG_EBP] = (reg8 *) &ebp;
      _mapping[REG_EFLAGS] = (reg8 *) &eflags;
      _mapping[REG_SEG_CS] = (reg8 *) &cs;
      _mapping[REG_SEG_DS] = (reg8 *) &ds;
      _mapping[REG_SEG_SS] = (reg8 *) &ss;
      _mapping[REG_SEG_ES] = (reg8 *) &es;
      _mapping[REG_SEG_FS] = (reg8 *) &fs;
      _mapping[REG_SEG_GS] = (reg8 *) &gs;

      _mapping[REG_AX] = (reg8 *) &(eax.parts[0]);
      _mapping[REG_BX] = (reg8 *) &(ebx.parts[0]);
      _mapping[REG_CX] = (reg8 *) &(ecx.parts[0]);
      _mapping[REG_DX] = (reg8 *) &(edx.parts[0]);
      _mapping[REG_SI] = (reg8 *) &(esi.parts[0]);
      _mapping[REG_DI] = (reg8 *) &(edi.parts[0]);
      _mapping[REG_SP] = (reg8 *) &(esp.parts[0]);
      _mapping[REG_BP] = (reg8 *) &(ebp.parts[0]);

      _mapping[REG_AL] = &(eax.parts[0].parts[0]);
      _mapping[REG_AH] = &(eax.parts[0].parts[0]);
      _mapping[REG_BL] = &(eax.parts[0].parts[0]);
      _mapping[REG_BH] = &(eax.parts[0].parts[0]);
      _mapping[REG_CL] = &(eax.parts[0].parts[0]);
      _mapping[REG_CH] = &(eax.parts[0].parts[0]);
      _mapping[REG_DL] = &(eax.parts[0].parts[0]);
      _mapping[REG_DH] = &(eax.parts[0].parts[0]);

      clearAll();

   }

   // Access the elements of the cache given a Pin register name.
   reg32 &elem32(REG r) { return *((reg32 *) _mapping[r]); }
   reg16 &elem16(REG r) { return *((reg16 *) _mapping[r]); }
   reg8 &elem8(REG r) { return *(_mapping[r]); }

   void setAll(uint32_t eax, uint32_t ebx, uint32_t ecx, uint32_t edx,
               uint32_t esi, uint32_t edi, uint32_t esp, uint32_t ebp,
               uint32_t eflags,
               uint16_t cs, uint16_t ds, uint16_t ss,
               uint16_t es, uint16_t fs, uint16_t gs)
   {
      this->eax.full = eax;
      this->ebx.full = ebx;
      this->ecx.full = ecx;
      this->edx.full = edx;
      this->esi.full = esi;
      this->edi.full = edi;
      this->esp.full = esp;
      this->ebp.full = ebp;
      this->eflags.full = eflags;
      
      this->cs.full = cs;
      this->ds.full = ds;
      this->ss.full = ss;
      this->es.full = es;
      this->fs.full = fs;
      this->gs.full = gs;

   }

   void clearAll()
   {

      eax.full = 0;
      ebx.full = 0;
      ecx.full = 0;
      edx.full = 0;
      esi.full = 0;
      edi.full = 0;
      esp.full = 0;
      ebp.full = 0;
      eflags.full = 0;
      cs.full = 0;
      ds.full = 0;
      ss.full = 0;
      es.full = 0;
      fs.full = 0;
      gs.full = 0;
      catchall.full = 0;
      
   }

};

#define MEMCACHE_SIZE 4096
#define MEMCACHE_MASK 0xfff

struct MemCache {
   uint8_t cache[MEMCACHE_SIZE];
   
   MemCache() { clearAll(); }

   // Access the cache entry for a given address.
   uint32_t &elem32(uint32_t addr)
   { return *((uint32_t *) &cache[addr & MEMCACHE_MASK]); }

   uint16_t &elem16(uint32_t addr)
   { return *((uint16_t *) &cache[addr & MEMCACHE_MASK]); }

   uint8_t &elem8(uint32_t addr)
   { return cache[addr & MEMCACHE_MASK]; }

   void clearAll()
   { memset((void *) cache, 0, MEMCACHE_SIZE * sizeof(uint8_t)); }

};
