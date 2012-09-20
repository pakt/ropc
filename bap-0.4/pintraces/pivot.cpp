/**
   @author Edward J. Schwartz

   $Id: pivot.cpp 5128 2011-09-09 20:39:57Z swhitman $
   
   This module contains code to test if various pivot gadgets will be
   effective.  A list of gadgets is given as input.  The target of each
   gadget is checked to see if it is tainted.  If so, that gadget is a
   potential pivot, since it transfers control to user-modifiable memory.
*/

#include "pivot.h"
#include "pin.H"
#include <cassert>
#include <fstream>
#include <iostream>

using namespace pintrace;

/** Pivot comparison */
bool operator<(const struct pivot_s &a, const struct pivot_s &b) {
  return (&a < &b);
}


static pivottype_t PIVOT_parse_pivottype(string s) {
  if (s == "switchstack") return SWITCHSTACK;
  if (s == "mempivot") return MEMPIVOT;
  cerr << "Invalid pivot type: " << s << endl;
  return UNKNOWN;
}

static const string switchstack_str = string("Switchstack");
static const string mempivot_str = string("Mempivot");
static const string unknown_str = string("Unknown");

static string PIVOT_pivottype_string(pivottype_t t) {
  switch (t) {
      case SWITCHSTACK:
        return switchstack_str;
      case MEMPIVOT:
        return mempivot_str;
      default:
        return unknown_str;
  }

  return string("Invalid");
}

/**
   Convert a few REGs to strings.
*/
#define CASE(r) case REG_##r: return string(#r); break;
static string PIVOT_reg_string(REG ri) {
  switch(ri) {
    CASE(EAX);
    CASE(EBX);
    CASE(ECX);
    CASE(EDX);
    CASE(ESI);
    CASE(EDI);
    CASE(ESP);
    CASE(EBP);

      default:
        cerr << "Unknown REG: " << ri << endl;
        assert(false);
        break;
  }
  return string("Invalid");
}
#undef CASE

/**
   Convert a few strings to REGs
*/
#define CASE(r) if (ri == "R_" #r) { return REG_##r; }
static REG PIVOT_string_reg(string ri) {
  CASE(EAX);
  CASE(EBX);
  CASE(ECX);
  CASE(EDX);
  CASE(ESI);
  CASE(EDI);
  CASE(ESP);
  CASE(EBP);
  cerr << "Unknown register: " << ri << endl;
  return REG_INVALID_;
}
#undef CASE

/**
   Print pivot.
*/
ostream& operator<<(ostream &o, const pivot_s &p) {
  o << "@" << p.address << ": "
    << PIVOT_pivottype_string(p.t) << " "
    << PIVOT_reg_string(p.base) << " "
    << p.offset;

  return o;
}

/**
   Parses an input stream.  The file has format: <gadget type> <reg> <offset>.
*/
pivot_set PIVOT_parseinput(istream &f) {
  pivot_set ps; 

  while (f.good()) {
    pivot_t p;
    string s;


    /* Read gadget address. */
    f >> hex >> p.address;

    if (!f.good()) break;

    /* Set pivot type. */
    f >> s;
    p.t = PIVOT_parse_pivottype(s);
    if (p.t == UNKNOWN) {
      break;
    }

    /* Read register name. */
    f >> s;
    p.base = PIVOT_string_reg(s);
    
    /* Read offset. */
    f >> hex >> p.offset;

    /* Add pivot to pivot set. */
    ps.insert(p);
  }

  return ps; /* return by value; slow but easy */
}

/**
   Given a set of pivots, return the address of memory they will transfer to esp.
*/
void PIVOT_testpivot(pivot_set ps, CONTEXT *ctx, TaintTracker &tt) {
  pivot_set::iterator i;
  
  cerr << "gaddr" << "\tpivot" << "\teaddr" << "\tconsec" << "\ttval" << "\tmapped" << endl;
  for (i = ps.begin(); i != ps.end(); i++) {
    
    intptr_t a = PIN_GetContextReg(ctx, (*i).base);
    a += (*i).offset;
    
    /* Put the address of our bytes in a. */
    switch((*i).t) {
    case SWITCHSTACK:
      /* We want to look at reg + offset. */
      break;
    case MEMPIVOT:
      /* We want to look at M[reg + offset] +?. */
      intptr_t ptr;
      size_t bytes;

      bytes = PIN_SafeCopy(&ptr, (void*)a, sizeof(intptr_t));
      if (bytes == sizeof(intptr_t)) {
	a = ptr;
      }
      break;

    default:
      cerr << "Unknown pivot type" << endl;
      break;
    }
    /* Check if the bytes are tainted. */
    switch((*i).t) {
        case SWITCHSTACK:
        case MEMPIVOT:
        {
          char tempbuf[1];
          uint32_t t = tt.getMemTaint(a, VT_MEM8);
          size_t consecTainted = 0;
          uint32_t readable = PIN_SafeCopy(tempbuf, (void*)a, 1);
          
          for (uint32_t base = a; tt.getMemTaint(base, VT_MEM8) != NOTAINT; base++, consecTainted++) { 
            /* .... */
          }
          
          cerr << *i << "\t" << a << "\t" << consecTainted << "\t" << t << "\t" << readable << endl;
          break;
        }
          
        default:
          /* Impossible. */
          assert(false);
          break;
    }
  }  
}

/*parse input -> set of pivots

test pivots -> map of pivots to result

output pivots -> writes to file*/
