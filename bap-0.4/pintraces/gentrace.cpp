#include "pin.H"

#include <cassert>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stack>
#include <vector>
#include <cstring>
#include <stdint.h>
#include <time.h>

#include "pin_frame.h"
#include "pin_trace.h"
#include "cache.h"

#include "pin_frame.cpp"
#include "pin_trace.cpp"

#include "pivot.h"

#include "pin_taint.h"

const ADDRINT ehandler_fs_offset = 0;
const ADDRINT ehandler_nptr_offset = 0;
const ADDRINT ehandler_handler_offset = 4;
const ADDRINT ehandler_invalid_ptr = 0xFFFFFFFF;
const ADDRINT ehandler_size = 8;
/** The offset esp has from when the exception is initially handled to
    when the handler is called. */
const ADDRINT ehandler_esp_offset = 0xe0;

const int maxSehLength = 10;

#ifdef _WIN32

const char* const windowsDll = "kernel32.dll";
const char* const wsDll = "WS2_32.dll";

const int callbackNum = 5;

const unsigned int accessViolation = 0xc0000005;

namespace WINDOWS {
#include "Winsock2.h"
#include "Windows.h"
}
#endif

/* Environment variables on windows
 * 
 * For a program that uses getenv, Windows does the following:
 *
 * 1. Call GetEnvironmentStringsW and set up an environment table.
 * 2. If using main (rather than wmain), WideCharToMultiByte is used
 * to convert to a multibyte environment table.
 * 
 * WideCharToMultiByte is implemented using a conversion table; we
 * don't handle control-flow taint, and thus we cannot really handle
 * tainting of environment variables :-/
 */

/* Networking on windows
 * 
 * Winsock appears to communicate with a windows subsystem using a
 * lightweight procedure calling interface, e.g., something we don't
 * want to parse.  So, we catch sockets() by instrumenting the socket
 * call itself.
 */

//
// CONFIGURATION
//

// Note: since we only flush the buffer once per basic block, the number
// of instructions per block should never exceed BUFFER_SIZE.
// TODO: See if there's some way to overcome this limitation, if
// necessary.
#define BUFFER_SIZE 10240

// Add a keyframe every KEYFRAME_FREQ instructions.
#define KEYFRAME_FREQ 10240

// Use value caching.
#define USE_CACHING

// Use faster functions to append to the value buffer, where possible.
//#define USE_FASTPATH

#ifdef USE_FASTPATH
#define _FASTPATH true
#else
#define _FASTPATH false
#endif

/** Set to 1 to enable lock debug information */
#ifndef DEBUG_LOCK
#define DEBUG_LOCK 0
#endif

KNOB<string> KnobOut(KNOB_MODE_WRITEONCE, "pintool",
                     "o", "out.bpt",
                     "Trace file to output to.");

KNOB<int> KnobTrigAddr(KNOB_MODE_WRITEONCE, "pintool",
                       "trig_addr", "",
                       "Address of trigger point. No logging will occur until execution reaches this address.");

KNOB<string> KnobTrigModule(KNOB_MODE_WRITEONCE, "pintool",
                            "trig_mod", "",
                            "Module that trigger point is in.");

KNOB<int> KnobTrigCount(KNOB_MODE_WRITEONCE, "pintool",
                        "trig_count", "0",
                        "Number of times trigger will be executed before activating.");

//
// NOTE: This limit is not a hard limit; the generator only stops logging
// during buffer flushes, so the actual number of instructions logged
// might exceed log_limit, but at the most by BUFFER_SIZE.
// Also note that the limit is in terms of the number of _instructions_,
// not frames; things like keyframes, LoadModuleFrames, etc. are not
// included in the count.
//
KNOB<uint64_t> KnobLogLimit(KNOB_MODE_WRITEONCE, "pintool",
                            "log-limit", "0",
                            "Number of instructions to limit logging to.");

KNOB<bool> LogAllSyscalls(KNOB_MODE_WRITEONCE, "pintool",
		       "log-syscalls", "false",
		       "Log system calls (even those unrelated to taint)");

KNOB<bool> KnobTaintTracking(KNOB_MODE_WRITEONCE, "pintool",
                             "taint-track", "true", 
                             "Enable taint tracking");

KNOB<bool> LogAllAfterTaint(KNOB_MODE_WRITEONCE, "pintool",
                           "logall-after", "false", 
                           "Log all (even untainted) instructions after the first tainted instruction");

KNOB<bool> LogAllBeforeTaint(KNOB_MODE_WRITEONCE, "pintool",
                           "logall-before", "false", 
                           "Log all (even untainted) instructions before and after the first tainted instruction");

// This option logs one instruction.  It then generates a fake
// standard frame to include operands after the instruction executed.
KNOB<bool> LogOneAfter(KNOB_MODE_WRITEONCE, "pintool",
                  "logone-after", "false",
                  "Log the first instruction outside of the log range (taint-start/end), and then exit.");

KNOB<string> TaintedFiles(KNOB_MODE_APPEND, "pintool",
                          "taint-files", "", 
                          "Consider the given files as being tainted");

KNOB<bool> TaintedArgs(KNOB_MODE_WRITEONCE, "pintool",
                       "taint-args", "false", 
                       "Command-line arguments will be considered tainted");

KNOB<bool> TaintedStdin(KNOB_MODE_WRITEONCE, "pintool",
                        "taint-stdin", "false", 
                        "Everything read from stdin will be considered tainted");

KNOB<bool> TaintedNetwork(KNOB_MODE_WRITEONCE, "pintool",
                          "taint-net", "false", 
                          "Everything read from network sockets will be considered tainted");

KNOB<bool> TaintedIndices(KNOB_MODE_WRITEONCE, "pintool",
                          "taint-indices", "false", 
                          "Values loaded with tainted memory indices will be considered tainted");

// FIXME: we should be able to specify more refined tainted 
// sources, e.g., that only the 5th argument should be considered
// tainted
KNOB<string> TaintedEnv(KNOB_MODE_APPEND, "pintool",
                      "taint-env", "", 
                      "Environment variables to be considered tainted");

KNOB<uint32_t> TaintStart(KNOB_MODE_WRITEONCE, "pintool",
			"taint-start", "0x0", 
			"All logged instructions will have higher addresses");

KNOB<uint32_t> TaintEnd(KNOB_MODE_WRITEONCE, "pintool",
                      "taint-end", "0xffffffff", 
                      "All logged instructions will have lower addresses");

KNOB<string> FollowProgs(KNOB_MODE_APPEND, "pintool",
                          "follow-progs", "", 
                          "Follow the given program names if they are exec'd");

KNOB<string> PivotFile(KNOB_MODE_WRITEONCE, "pintool",
		       "pivots-file", "",
		       "Load file of pivot gadgets");

KNOB<bool> SEHMode(KNOB_MODE_WRITEONCE, "pintool",
		   "seh-mode", "false",
		   "Record an SEH exploits");

KNOB<int> CheckPointFreq(KNOB_MODE_WRITEONCE, "pintool",
			 "freq", "10000",
			 "Report value of eip every n instructions.");

KNOB<int> CacheLimit(KNOB_MODE_WRITEONCE, "pintool",
		    "cache-limit", "500000000",
		    "Code-cache size limit (bytes)");

KNOB<int> SkipTaints(KNOB_MODE_WRITEONCE, "pintool",
		     "skip-taints", "0",
		     "Skip this many taint introductions");

struct FrameBuf {
   uint32_t addr;
   uint32_t tid;
   uint32_t insn_length;

   // The raw instruction bytes will be stored as 16 bytes placed over 4
   // integers. The conversion is equivalent to the casting of a char[16]
   // to a uint32_t[4].
   // NOTE: This assumes that MAX_INSN_BYTES == 16!!!
   uint32_t rawbytes0;
   uint32_t rawbytes1;
   uint32_t rawbytes2;
   uint32_t rawbytes3;

   uint32_t values_count;
   ValSpecRec valspecs[MAX_VALUES_COUNT];

};

/**
 * Temporary structure used during instrumentation.
 */
typedef struct TempOps_s {
  uint32_t reg;
  uint32_t type;
  uint32_t taint;
} TempOps_t;

/**
 * Posible ways of passing a register to an analysis function
 */
enum RPassType { P_VALUE, P_REF, P_CONTEXT };

/**
 * Given a register, decide how to pass it.
 */
static RPassType howPass(REG r) {

  /* XMM registers can be passed by reference */
  if (REG_is_xmm(r) || REG_is_ymm(r) || REG_is_mm(r))
    return P_REF;
  
  // For now, let's just use context
  return P_CONTEXT;
}

/**
 * Avoiding logging some addresses.
 */
static bool dontLog(ADDRINT addr) {

  IMG i = IMG_FindByAddress(addr);
  if (IMG_Valid(i)) {

    char tempbuf[BUFSIZE];
    char *tok = NULL;
    char *lasttok = NULL;
    
    // Fill up the temporary buffer
    strncpy(tempbuf, IMG_Name(i).c_str(), BUFSIZE);
    
    // We don't need a lock, since this is an instrumentation function (strtok is not re-entrant)
    strtok(tempbuf, "\\");
    
    while ((tok = strtok(NULL, "\\")) != NULL) {
      // Just keep parsing...
      lasttok = tok;
    }
    
    if (lasttok) {
      if (lasttok == string("uxtheme.dll")) {
	return true;
      }
    }
  }

  return false;
}



/**
 * This type preserves state between a system call entry and exit.
 */
typedef struct SyscallInfo_s {

  /** Frame for system call */
  SyscallFrame sf;

  /** State shared between taintIntro and taintStart */
  uint32_t state;
} SyscallInfo_t;

/**
 * This type preserves state between a recv() call and return
 */
typedef struct RecvInfo_s {
  /** Fd */
  uint32_t fd;

  /** The address */
  void* addr;

  /** Bytes written ptr. */
  uint32_t *bytesOut;
} RecvInfo_t;

/**
 * Thread local information
 */

typedef struct ThreadInfo_s {
  // Stack keeping track of system calls
  // Needed because windows system calls can be nested!
  std::stack<SyscallInfo_t> scStack;
  std::stack<RecvInfo_t> recvStack;
  context delta;
} ThreadInfo_t;

int g_counter = 0;

TraceWriter *g_tw;

// A taint tracker
TaintTracker * tracker;

// Vector used to collect TOC entries.
vector<uint32_t> g_toc;

FrameBuf g_buffer[BUFFER_SIZE];
uint32_t g_bufidx;

// Counter to keep track of when we should add a keyframe.
uint32_t g_kfcount;

// Caches.
//RegCache g_regcache;
//MemCache g_memcache;

// Profiling timer.
clock_t g_timer;

// True if logging is activated.
// Logging should be activated if it is possible for some instruction
// to be logged.  This could happen because 1) we are logging all
// instructions, or 2) taint is introduced, and so the instruction
// could be tainted.
bool g_active;

// Number of instructions logged so far.
uint64_t g_logcount;

// Number of instructions to limit logging to.
uint64_t g_loglimit;

// True if a trigger was specified.
bool g_usetrigger;

// Activate taint analysis
// bool t_active;

// Whether taint has been introduced
bool g_taint_introduced;

// True if the trigger address was resolved.
bool g_trig_resolved;

uint32_t g_trig_addr;

// We use a signed integer because sometimes the countdown will be
// decremented past zero.
int g_trig_countdown;

// Name of our thread/process
char g_threadname[BUFFER_SIZE] = "s";

// A lock on any shared state
PIN_LOCK lock;

// An environment to keep all the values
ValSpecRec values[MAX_VALUES_COUNT];

// Address ranges
uint32_t start_addr, end_addr;

// Pivot set
pivot_set ps;

// Exit after the next instruction
bool g_exit_next;

// Prototypes.
VOID Cleanup();

// Key for thread local system call stack
static TLS_KEY tl_key;


// Start of functions.

VOID ModLoad(IMG i, void*);

// Get Thread Info
ThreadInfo_t* GetThreadInfo(void) {
  ThreadInfo_t* ti;

  ti = static_cast<ThreadInfo_t*> (PIN_GetThreadData(tl_key));
  assert(ti);
  return ti;
}

// Create a new thread information block for the current thread
ThreadInfo_t* NewThreadInfo(void) {
  ThreadInfo_t* ti = NULL;

  ti = new ThreadInfo_t;
  assert(ti);

  PIN_SetThreadData(tl_key, ti, PIN_ThreadId());

  return ti;
}

/** Given a REG, return a trace type (or VT_NONE for failure) */
static uint32_t GetTypeOfReg(REG r) {
  if (REG_is_gr8(r)) return VT_REG8;
  if (REG_is_gr16(r)) return VT_REG16;
  if (REG_is_gr32(r)) return VT_REG32;
  if (REG_is_gr64(r)) return VT_REG64;

  string s = REG_StringShort(r);

  if (s == "eip" || s == "eflags") {
    // No problem for these
    return VT_REG32;
  }

  // Otherwise, print a warning...
  
  cerr << "Warning: Unknown register size of register " << REG_StringShort(r) << endl;
  return VT_NONE;
}

static uint32_t GetRegType(INS ins, uint32_t i) {
  switch (INS_OperandWidth(ins, i)) {
      case 128:
        return VT_REG128;
        break;
      case 64:
        return VT_REG64;
        break;
      case 32:
        return VT_REG32;
        break;
      case 16:
        return VT_REG16;
        break;
      case 8:
        return VT_REG8;
        break;
      default:       
        cerr << "Unsupported register width: " << INS_OperandWidth(ins, i) << endl;
        cerr << "Instruction: " << INS_Disassemble(ins) << endl;
        cerr << "Category: " << CATEGORY_StringShort(INS_Category(ins)) << endl;
        assert(false);
        break;
  }

  return -1;
}

static uint32_t GetByteSize(uint32_t vtype) {
  switch(vtype) {
  case VT_MEM256:
    return 32;

  case VT_REG128:
  case VT_MEM128:
    return 16;
    
  case VT_REG64:
  case VT_MEM64:
    return 8;

  case VT_REG32:
  case VT_MEM32:
    return 4;
    
  case VT_REG16:
  case VT_MEM16:
    return 2;
    
  case VT_REG8:
  case VT_MEM8:
    return 1;
  }
  
  cerr << "Unknown type " << vtype << endl;
  assert(false);
  return -1;
}

void LLOG(const char *str) {
#if DEBUG_LOCK
  LOG(str);
#else
  /* Disabled */
#endif
}

ADDRINT CheckTrigger()
{
   return --g_trig_countdown <= 0;
}

/** Reinstrument all images. XXX: Remove me. */
VOID InstrumentIMG() {
  PIN_LockClient();
  for (IMG i = APP_ImgHead(); IMG_Valid(i); i = IMG_Next(i)) {
    ModLoad(i, (void*)1);
  }
  PIN_UnlockClient();
}

VOID Activate(CONTEXT *ctx)
{
  cerr << "Activating logging" << endl;
  g_active = true;
  PIN_RemoveInstrumentation();
  PIN_ExecuteAt(ctx);
}

/** Activate taint analysis.

    Note: It's important to NOT hold locks when calling this function.
    PIN_RemoveInstrumentation obtains the VM lock, which is only possible
    when no analysis functions/etc are executing.  If one is waiting for
    one of our locks, this will cause a deadlock.  
*/
VOID TActivate()
{
  cerr << "Activating taint analysis " << endl;
  g_active = true; /* Any instruction could be logged because taint is
                      introduced. */
  g_taint_introduced = true; /* Taint is definitely introduced now. */
  PIN_RemoveInstrumentation();
  InstrumentIMG();
}

//
// Returns true if the buffer index with count added to it exceeds the
// maximum size of the buffer.
//
ADDRINT CheckBuffer(UINT32 count)
{
  return (g_bufidx + count) >= BUFFER_SIZE;

}

ADDRINT CheckBufferEx(BOOL cond, UINT32 count, UINT32 count2)
{
   return cond && ((g_bufidx + count + count2) >= BUFFER_SIZE);
}

// Callers must ensure mutual exclusion
VOID FlushInstructions()
{

  for(uint32_t i = 0; i < g_bufidx; i++) {

      StdFrame2 f;
      f.addr = g_buffer[i].addr;
      f.tid = g_buffer[i].tid;
      f.insn_length = g_buffer[i].insn_length;

      //LOG(hexstr(f.addr) + ": writing.\n");

      if (f.insn_length > MAX_INSN_BYTES) {
         LOG("Error! f.insn_length is too long!\n");
      }

      memcpy((char *) f.rawbytes, (char *) &(g_buffer[i].rawbytes0), f.insn_length);

      f.clearCache();

      uint32_t newcnt = 0;

      // Go through each value and remove the ones that are cached.

      for (uint32_t j = 0; j < g_buffer[i].values_count; j++) {

         ValSpecRec &v = g_buffer[i].valspecs[j];

         // Copying types, usage, location, and taint is always the same.
         f.types[newcnt] = v.type;
         f.usages[newcnt] = v.usage;
         f.locs[newcnt] = v.loc;
         f.taint[newcnt] = v.taint;

         // Now copying the value is the same, too.  But, we could be
         // more efficient and copy less bytes...
         memcpy(&(f.values[newcnt]), &(v.value), sizeof(LEVEL_VM::PIN_REGISTER));
         

         switch (v.type) {

	 case VT_MEM256:
	 case VT_REG128:
	 case VT_MEM128:
	 case VT_REG64:
	 case VT_MEM64:
	   //cerr << "WARN: Incomplete code" << endl;
	   break;
	   
	 case VT_REG32:
	 case VT_REG16:
	 case VT_REG8:
	 case VT_MEM32:
	 case VT_MEM16:
	 case VT_MEM8:
	   break;
               
         default:
            // We're in trouble if we don't know the type.

           assert(false);
           
         }

         newcnt++;
         
      }


      f.values_count = newcnt;

      g_tw->add(f);

   }

   // Update counts.
   g_logcount += g_bufidx;
   g_kfcount += g_bufidx;

   g_bufidx = 0;

}

//
// Writes all instructions stored in the buffer to disk, and resets the
// buffer index to 0. Also checks to see if we need to insert a
// keyframe. If so, inserts the keyframe using the data in the supplied
// context.
//
VOID FlushBuffer(BOOL addKeyframe, const CONTEXT *ctx, THREADID threadid, BOOL needlock)
{

  LLOG("Begin flushing buffer.\n");

  if (needlock) {
    GetLock(&lock, threadid+1);
  }

  FlushInstructions();

   
   // Check to see if we should insert a keyframe here.
   if (addKeyframe && (g_kfcount >= KEYFRAME_FREQ)) {

      //LOG("Inserting keyframe:\n");
      //LOG("  addr: " + hexstr(PIN_GetContextReg(ctx, REG_EIP)) + "\n");

     assert(ctx);

      KeyFrame kf;
      kf.pos = g_tw->count();
      kf.setAll((uint32_t) PIN_GetContextReg(ctx, REG_EAX),
                (uint32_t) PIN_GetContextReg(ctx, REG_EBX),
                (uint32_t) PIN_GetContextReg(ctx, REG_ECX),
                (uint32_t) PIN_GetContextReg(ctx, REG_EDX),
                (uint32_t) PIN_GetContextReg(ctx, REG_ESI),
                (uint32_t) PIN_GetContextReg(ctx, REG_EDI),
                (uint32_t) PIN_GetContextReg(ctx, REG_ESP),
                (uint32_t) PIN_GetContextReg(ctx, REG_EBP),
                (uint32_t) PIN_GetContextReg(ctx, REG_EFLAGS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_CS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_DS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_SS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_ES),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_FS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_GS)
                );


      // Add entry to TOC.
      g_toc.push_back(g_tw->offset());

      // And then add the keyframe.
      g_tw->add(kf);

      // // Now repopulate the register cache.
      // g_regcache.setAll(kf.eax, kf.ebx, kf.ecx, kf.edx,
      //                   kf.esi, kf.edi, kf.esp, kf.ebp,
      //                   kf.eflags,
      //                   kf.cs, kf.ds, kf.ss,
      //                   kf.es, kf.fs, kf.gs);
                        
      // // Finally clear the data cache.
      // g_memcache.clearAll();

      g_kfcount = 0;

   }

   // See if we've gotten sufficient instructions.
   if ((g_loglimit != 0) && (g_logcount >= g_loglimit)) {
     LOG("Logged required number of instructions, quitting.\n");
     cerr << "Logged required number of instructions, quitting." << endl;
     Cleanup();
     //ReleaseLock(&lock);
     // Never release lock
     //PIN_Detach();
     exit(0);
   } else {
     if (needlock) {
       ReleaseLock(&lock);
     }
   }


   
   LLOG("End flushing buffer.\n");

}

#ifdef _WIN32

/** Wrapper for accept */
uint32_t AcceptWrapper(CONTEXT *ctx, AFUNPTR fp, THREADID tid, uint32_t s, void *addr, int *addrlen) {

  cerr << "AcceptWrapper" << endl;

  uint32_t ret;

  PIN_CallApplicationFunction(ctx, tid,
			      CALLINGSTD_STDCALL, fp,
			      PIN_PARG(uint32_t), &ret,
			      PIN_PARG(uint32_t), s,
			      PIN_PARG(void*), addr,
			      PIN_PARG(int*), addrlen,
			      PIN_PARG_END());

  GetLock(&lock, tid+1);
  tracker->acceptHelper(ret);
  ReleaseLock(&lock);

  return ret;
			      
}

/** Wrapper for WSAConnect */
uint32_t WSAConnectWrapper(CONTEXT *ctx, AFUNPTR fp, THREADID tid, uint32_t s, void *arg2, void *arg3, void *arg4, void *arg5, void *arg6, void *arg7) {

  cerr << "WSAConnectWrapper" << endl;

  uint32_t ret;

  cerr << "Connect to socket " << s << endl;

  PIN_CallApplicationFunction(ctx, tid,
			      CALLINGSTD_STDCALL, fp,
			      PIN_PARG(uint32_t), &ret,
			      PIN_PARG(uint32_t), s,
			      PIN_PARG(void*), arg2,
			      PIN_PARG(void*), arg3,
			      PIN_PARG(void*), arg4,
			      PIN_PARG(void*), arg5,
			      PIN_PARG(void*), arg6,
			      PIN_PARG(void*), arg7,
			      PIN_PARG_END());

  GetLock(&lock, tid+1);
  if (ret != SOCKET_ERROR) {
    tracker->acceptHelper(s);
  } else {
    cerr << "WSAConnect error " << ret << endl;
  }
  ReleaseLock(&lock);

  return ret;
			      
}

/** Wrapper for connect */
uint32_t ConnectWrapper(CONTEXT *ctx, AFUNPTR fp, THREADID tid, uint32_t s, void *arg2, void *arg3) {

  cerr << "ConnectWrapper" << endl;

  uint32_t ret;

  cerr << "Connect to socket " << s << endl;

  PIN_CallApplicationFunction(ctx, tid,
			      CALLINGSTD_STDCALL, fp,
			      PIN_PARG(uint32_t), &ret,
			      PIN_PARG(uint32_t), s,
			      PIN_PARG(void*), arg2,
			      PIN_PARG(void*), arg3,
			      PIN_PARG_END());

  GetLock(&lock, tid+1);
  //  if (ret != SOCKET_ERROR) {
  // Non-blocking sockets will return an "error".  However, we can't
  // call GetLastError to find out what the root problem is,
  // so... we'll just assume the connection was successful.
    tracker->acceptHelper(s);
 
 // } else {
 //    cerr << "connect error " << ret << endl;
 //  }
  ReleaseLock(&lock);

  return ret;
			      
}

void BeforeRecv(THREADID tid, uint32_t s, char* buf) {

  RecvInfo_t r;

  r.fd = s;
  r.addr = buf;
  r.bytesOut = NULL;

  ThreadInfo_t *ti = GetThreadInfo();

  ti->recvStack.push(r);
}

void WSABeforeRecv(THREADID tid, uint32_t s, WINDOWS::LPWSABUF bufs, WINDOWS::LPDWORD bytesOut) {

  RecvInfo_t r;

  r.fd = s;
  r.addr = bufs[0].buf;
  r.bytesOut = (uint32_t*) bytesOut;

  ThreadInfo_t *ti = GetThreadInfo();

  ti->recvStack.push(r);
}

void AfterRecv(THREADID tid, int ret, char *f) {
  cerr << "afterrecv called by " << f << endl;
  ThreadInfo_t *ti = GetThreadInfo();
  uint32_t len = 0;

  if (ti->recvStack.empty()) {
    cerr << "WARNING: Stack empty in AfterRecv(). Thread " << tid << endl;
  } else {
  
    RecvInfo_t ri = ti->recvStack.top();
    ti->recvStack.pop();
    
    if (ret != SOCKET_ERROR) {
      GetLock(&lock, tid+1);
      //cerr << "fd: " << ri.fd << endl;

      uint32_t numbytes = 0;
      if (ri.bytesOut) {
	numbytes = *(ri.bytesOut);
      } else {
	numbytes = ret;
      }

      std::vector<TaintFrame> tfs = tracker->recvHelper(ri.fd, ri.addr, numbytes);
      ReleaseLock(&lock);
      
      if (tfs.size() > 0) {
	
	if (!g_taint_introduced) {
	  TActivate();
	}
	
	GetLock(&lock, tid+1);
	g_tw->add(tfs);
	ReleaseLock(&lock);
      }
    } else {
      cerr << "recv() error " << endl;
    }
  }
}


/** Wrapper for calling GetEnvironmentStringsW() and tainting the output */
void* GetEnvWWrap(CONTEXT *ctx, AFUNPTR fp, THREADID tid) {
  void *ret = NULL;

  /* 
     We must lock after the PIN_CallApplicationFunction call, since
     the called code is instrumented, and also tries to obtain the
     lock. 
     
     This probably is not a big deal, but theoretically the
     instrumented code in another thread could change the memory as
     we're reading it.  This seems pretty unlikely.  If we ever feel
     like fixing it, we could obtain PIN's global vm lock.
  */

  PIN_CallApplicationFunction(ctx, tid,
  			      CALLINGSTD_STDCALL, fp,
  			      PIN_PARG(uint32_t), &ret,
  			      PIN_PARG_END());

  LLOG("Getting lock in callback\n");

  GetLock(&lock, tid+1);
  LLOG("Got callback lock\n");

  std::vector<TaintFrame> frms = tracker->taintEnv(NULL, (wchar_t*) ret);
  g_tw->add(frms);

  ReleaseLock(&lock);
  LLOG("Releasing callback lock\n");

  return ret;
}

/** Wrapper for calling GetEnvironmentStringsA() and tainting the output */
void* GetEnvAWrap(CONTEXT *ctx, AFUNPTR fp, THREADID tid) {
  void *ret = NULL;

  cerr << "In a wrap " << endl;

  /* 
     We must lock after the PIN_CallApplicationFunction call, since
     the called code is instrumented, and also tries to obtain the
     lock. 
     
     This probably is not a big deal, but theoretically the
     instrumented code in another thread could change the memory as
     we're reading it.  This seems pretty unlikely.  If we ever feel
     like fixing it, we could obtain PIN's global vm lock.
  */

  PIN_CallApplicationFunction(ctx, tid,
  			      CALLINGSTD_STDCALL, fp,
  			      PIN_PARG(uint32_t), &ret,
  			      PIN_PARG_END());

  LLOG("Getting lock in callback\n");

  GetLock(&lock, tid+1);
  LLOG("Got callback lock\n");

  std::vector<TaintFrame> frms = tracker->taintEnv((char*) ret, NULL);
  g_tw->add(frms);

  ReleaseLock(&lock);
  LLOG("Releasing callback lock\n");

  return ret;
}

#endif

/* This analysis function is called after the target instruction is
 * executed when using -logone-after. It transfer control back to the
 * same instruction to log its operands after execution. */
VOID PostInstruction(ADDRINT addr, CONTEXT *ctx) {
  g_exit_next = true;
  PIN_SetContextReg(ctx, REG_INST_PTR, addr);
  PIN_ExecuteAt(ctx);
}

VOID AppendBuffer(ADDRINT addr,
                  THREADID tid,
                  CONTEXT *ctx,
                  BOOL isBranch,
                  UINT32 insn_length,

                   UINT32 rawbytes0,
                   UINT32 rawbytes1,
                   UINT32 rawbytes2,
                   UINT32 rawbytes3,
                   
                  /* Type contains the type of the operand. Location
                   * specifies the base address for memory operands.
                   * For registers, this holds the ID of the
                   * register.  Value is used to pass register values
                   * by reference.  For memory operands, it holds the
                   * byte offset into memory.  For instance, a 32-bit
                   * memory operand is broken into four 8-bit operands
                   * with the same address (specified in location),
                   * but with different offsets (0, 1, 2, 3) in
                   * value.  Usage specifies how the operand is used
                   * (read, write, etc.) */                   

		  UINT32 values_count,
		  ...
                   )
{
  va_list va;
  va_start(va, values_count);

  static int firstTaint = true;
  REG r;

   //LOG("APPEND: " + hexstr(addr) + "\n");

  /* BUILD_VAL touches values, so we need the lock early. */

  /* Periodically report eip. */
  if ((g_counter++ % CheckPointFreq.Value()) == 0) {
    // PIN_LockClient();
    // IMG i = IMG_FindByAddress(addr);
    // PIN_UnlockClient();
    // cerr << "Checkpoint: Executing code at " << addr;
    // if (IMG_Valid(i)) {
    //   cerr << " (" << IMG_Name(i) << ")";
    // } 
    cerr << " thread " << tid
	 << "; " << g_counter << " instructions" << endl
	 << "Code cache size is " << CODECACHE_CodeMemUsed() << endl
	 << "Code cache limit is " << CODECACHE_CacheSizeLimit() << endl;
  }

  LLOG("big thing\n");
  
  GetLock(&lock, tid+1);
  
  LLOG("got big thing\n");

  for (unsigned int i = 0; i < values_count; i++) {
    values[i].type = va_arg(va, uint32_t);				
    values[i].loc = va_arg(va, uint32_t);				
    values[i].value.dword[0] = va_arg(va, uint32_t);			
    values[i].usage = va_arg(va, uint32_t);				
    if (tracker->isMem(values[i].type)) {				
      /* Add memory byte offset */					
      values[i].loc += values[i].value.dword[0];			
    }									    
  }

/* Perform taint propagation and checking */

   bool abort = false;
   bool log_addr = ((start_addr <= addr) && (addr <= end_addr)) || LogOneAfter.Value();
   bool log_all =
     ((LogAllAfterTaint.Value() && !firstTaint)
      || LogAllBeforeTaint.Value());
   uint32_t pretaint[MAX_VALUES_COUNT];
   LEVEL_VM::PIN_REGISTER *pr = NULL;
   ThreadInfo_t *ti = NULL;

   ti = GetThreadInfo();

   tracker->setCount(values_count);

   bool has_taint = tracker->hasTaint(ti->delta);

   if ((log_all || has_taint) && log_addr) {

     /* This instruction is tainted, or we're logging all
      * instructions */

     if (has_taint && firstTaint) {
       cerr << "First tainted instruction" << endl;
       LOG("First tainted instruction.\n");
       firstTaint = false;
     }

     // Mark everything as untainted
     for (uint32_t i = 0 ; i < values_count ; i++) 
       values[i].taint = 0;
     
     // Set taint values from taint context
      tracker->setTaintContext(ti->delta);

      // Record pretaint (this goes in the log)
      for (uint32_t i = 0 ; i < values_count ; i++) 
         pretaint[i] = values[i].taint;

      // Did this instruction propagate taint?
      //propagated_taint = tracker->propagatedTaint(isBranch);
      
      if (!isBranch)
	tracker->taintPropagation(ti->delta);

      // Taint checking
      abort = !tracker->taintChecking();
         
   //} FIXME: it there a case where the instruction contains taint
   //  but we do not need to log it?
   //if (log || (has_taint && propagated_taint)) {
   
   //cerr << "Logging instruction " << rawbytes0 << " " << rawbytes1 << endl;

      // Now, fill in the buffer with information
      
   g_buffer[g_bufidx].addr = addr;
   g_buffer[g_bufidx].tid = tid;
   g_buffer[g_bufidx].insn_length = insn_length;

   g_buffer[g_bufidx].rawbytes0 = rawbytes0;
   g_buffer[g_bufidx].rawbytes1 = rawbytes1;
   g_buffer[g_bufidx].rawbytes2 = rawbytes2;
   g_buffer[g_bufidx].rawbytes3 = rawbytes3;

   // tracker->printRegs();

   g_buffer[g_bufidx].values_count = values_count;
  
//g_buffer[g_bufidx].valspecs[i].taint = values[i].taint;             

   // Store information to the buffer

   for (unsigned int i = 0; i < values_count; i++) {

     g_buffer[g_bufidx].valspecs[i].type = values[i].type;		
     g_buffer[g_bufidx].valspecs[i].usage = values[i].usage;		
     g_buffer[g_bufidx].valspecs[i].loc = values[i].loc;			
     g_buffer[g_bufidx].valspecs[i].taint = pretaint[i];                  
     
     switch(values[i].type) {						
       
     case VT_REG128:                                                  
     case VT_REG64:                                                   
       /*cerr << "Let's see what happens" << endl;*/			
       
     case VT_REG32:                                                   
     case VT_REG16:                                                   
     case VT_REG8:                                                    
       /*r = REG_FullRegName((REG) valspec##i##_loc);*/		
       r = (REG)values[i].loc;					
       
       /* Find how we should access the register value */             
       switch(howPass(r)) {                                           
       case P_CONTEXT:                                            
	 g_buffer[g_bufidx].valspecs[i].value.dword[0] =          
	   PIN_GetContextReg(ctx, r);                             
	 break;                                                   
         
       case P_REF:                                                
	 pr = (LEVEL_VM::PIN_REGISTER*) values[i].value.dword[0];	
	 memcpy(&(g_buffer[g_bufidx].valspecs[i].value),          
		pr,                                               
		sizeof(LEVEL_VM::PIN_REGISTER));                  
	 break;                                                   
	 
       default:                                                   
	 assert(false);                                           
	 break;                                                   
       }                                                              
       break;                                                         
       
     case VT_MEM256:							
     case VT_MEM128:							
     case VT_MEM64:							
     case VT_MEM32:							
     case VT_MEM16:							
     case VT_MEM8:							
       PIN_SafeCopy((VOID*) &(g_buffer[g_bufidx].valspecs[i].value),	
		    (const VOID *)(g_buffer[g_bufidx].valspecs[i].loc),	
		    GetByteSize(values[i].type));				
       break;								
     default:								
       cerr << "Large operands not fully supported yet, addr "        
	    << addr << endl;                                          
       assert(false);                                                 
       break;                                                         
     }
     
     //   cerr << "Building val specs now" << endl;
   
   }

   //   cerr << "... done" << endl;
   
   g_bufidx++;
   }

   
   /* For a non-SEH exploit, stop if taint checking fails.  In an SEH
      exploit, we may want an exception to trigger (e.g., from
      returning to a bad address). */
   if (abort && !SEHMode.Value()) { 
     pivot_set::iterator i;
     cerr << "Stack smashing detected! @" << addr << endl;
     cerr << "Exiting...." << endl;
     LOG("Stack smashing detected\n");

     /* Let's assume this was a ret for now and increment esp by
      * four.  Of course, this isn't correct in general.  XXX: Fix
      * this so it works for any last instruction. */
     ADDRINT esp = PIN_GetContextReg(ctx, REG_STACK_PTR);
     PIN_SetContextReg(ctx, REG_STACK_PTR, esp+4);
     
     PIVOT_testpivot(ps, ctx, *tracker);
     
     FlushBuffer(true, ctx, tid, false);
     Cleanup();
     exit(0);
   }

   if (g_exit_next) {
     FlushBuffer(true, ctx, tid, false);
     Cleanup();
     exit(0);
   }
   
   ReleaseLock(&lock);  
   LLOG("released big thing\n");

   va_end(va);

   return;

}

VOID InstrBlock(BBL bbl)
{

  // Now we need to get the values.
  
  uint32_t valcount;
  uint32_t icount = BBL_NumIns(bbl);
  
  // Used to temporarily store the values we obtain from the operands,
  // to faciliate further analysis for fast paths.
  TempOps_t opndvals[MAX_VALUES_COUNT];
  

   // LOG("INS: BBL start.\n");

   if (g_active) {

      if (icount > BUFFER_SIZE) {
         LOG("WARNING: Basic block too many instructions: " + 
             decstr(icount) + "\n");
         assert(false);
      }

      // Add instrumentation call to check if buffer needs to be flushed.
      BBL_InsertIfCall(bbl, IPOINT_BEFORE,
                       (AFUNPTR) CheckBuffer,
                       IARG_UINT32, icount,
                       IARG_END);

      BBL_InsertThenCall(bbl, IPOINT_BEFORE,
                         (AFUNPTR) FlushBuffer,
                         IARG_BOOL, true,
                         IARG_CONTEXT,
                         IARG_THREAD_ID,
			 IARG_BOOL, true,
                         IARG_END);

   }

   //LOG("INS: BBL ins start.\n");

   // Count of instructions that have yet to be inserted into the buffer,
   // at the point at which the current instruction will be executed.
   uint32_t insLeft = icount;

   for (INS ins = BBL_InsHead(bbl); INS_Valid(ins); ins = INS_Next(ins)) {

      if (!g_active && g_usetrigger) {
         // Logging has not been activated yet, so all we need to do now
         // is check for the trigger condition.

         if (INS_Address(ins) == g_trig_addr) {
            // Found the trigger address.

            INS_InsertIfCall(ins, IPOINT_BEFORE,
                             (AFUNPTR) CheckTrigger,
                             IARG_END);

            INS_InsertThenCall(ins, IPOINT_BEFORE,
                               (AFUNPTR) Activate,
                               IARG_CONTEXT,
                               IARG_END);

         }

         // Skip the rest of the analysis and immediately go on to the
         // next instruction.
         continue;

      }

      // Skip instrumentation unless g_active is enabled
      if (!g_active) {
        continue;
      }

      // Add instrumentation call to insert instruction into buffer.

      if (INS_Category(ins) == XED_CATEGORY_X87_ALU) {

         // TODO: Handle floating point instructions.
         LOG("Not logging FP instruction.\n");

	 cerr << "Not logging FP instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
         continue;

      } else if (INS_Category(ins) == XED_CATEGORY_PREFETCH) {
        LOG("Not logging prefetch instruction.\n");
	cerr << "Not logging prefetch instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
        continue;
      } else if (INS_Category(ins) == XED_CATEGORY_MMX) {
        LOG("Not logging mmx instruction.\n");
	cerr << "Not logging mmx instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
	continue;
      } else if (INS_Category(ins) == XED_CATEGORY_FCMOV) {
        LOG("Not logging float move instruction.\n");
	cerr << "Not logging float move instruction @" << INS_Address(ins) << ": " << INS_Disassemble(ins) << endl;
	continue;
      }

      // Check if there's a REP prefix.
      if (INS_HasRealRep(ins)) {

         INS_InsertIfCall(ins, IPOINT_BEFORE,
                          (AFUNPTR) CheckBufferEx,
                          IARG_FIRST_REP_ITERATION,
                          IARG_REG_VALUE, INS_RepCountRegister(ins),
                          IARG_UINT32, insLeft,
                          IARG_END);

         INS_InsertThenCall(ins, IPOINT_BEFORE,
                            (AFUNPTR) FlushBuffer,
                            IARG_BOOL, false,
                            IARG_ADDRINT, 0,
                            IARG_THREAD_ID,
			    IARG_BOOL, true,
                            IARG_END);

      }

      // The argument list to be passed into the instruction analysis call.
      IARGLIST arglist = IARGLIST_Alloc();
      IARGLIST arglist_helper = IARGLIST_Alloc();
      valcount = 0;
      
      // The first few arguments to AppendBuffer.
      IARGLIST_AddArguments(arglist,
                            IARG_ADDRINT, INS_Address(ins),
                            IARG_THREAD_ID,
                            IARG_CONTEXT,
                            IARG_BOOL, INS_IsBranch(ins),
                            IARG_UINT32, INS_Size(ins),
                            IARG_END);

      // Now we need to gather the instruction bytes.

      // Wastes a copy, but required because the instruction may not be
      // 32-bit aligned, and we want to respect word alignment requirements.
      uint32_t rawbytes_i[4];
      // Is it an xor?
      bool is_xor = false;

      UINT sz = INS_Size(ins);
      assert(PIN_SafeCopy((void*)rawbytes_i, (const void*) INS_Address(ins), sz) == sz);
      
      IARGLIST_AddArguments(arglist,
                            IARG_UINT32, rawbytes_i[0],
                            IARG_UINT32, rawbytes_i[1],
                            IARG_UINT32, rawbytes_i[2],
                            IARG_UINT32, rawbytes_i[3],
                            IARG_END);

      for (uint32_t i = 0; i < MAX_VALUES_COUNT; i++) {
        opndvals[i].taint = 0;
        opndvals[i].type = -1;
      }

      // specializing xors
      if (INS_Mnemonic(ins) == string("XOR") ||
	  INS_Mnemonic(ins) == string("PXOR")) {
	int opnum = -1;
	bool found = false;
	REG r = REG_INVALID();

	/* Find the source and destination operand. */
	for (uint32_t i = 0 ; i < INS_OperandCount(ins); i++) {
	  if (INS_OperandReadAndWritten (ins, i) &&
	      INS_OperandIsReg(ins, i)) {
	    } else {
	      found = true;
	      r = INS_OperandReg(ins, i);
	      opnum = -1;
	      break;
	    }
	  }

	/* Find the second operand, and ensure it's the same register
	   as the first operand we found. */
	if (found) {
	  for (uint32_t i = 0 ; i < INS_OperandCount(ins); i++) {
	    if (INS_OperandReadAndWritten (ins, i) &&
		INS_OperandIsReg(ins, i) &&
		r == INS_OperandReg(ins, i) &&
		(unsigned)opnum != i) {
	      is_xor = true;
	      break;
	    }
	  }
	}
      } /* end xor code */

      for(uint32_t i = 0; i < INS_OperandCount(ins); i++) {

	 if (INS_OperandRead(ins, i) && (!is_xor))
	   opndvals[valcount].taint = RD;
	 if (INS_OperandWritten(ins, i))
           opndvals[valcount].taint |= WR;
	
         /* Handle register operands */
         if (INS_OperandIsReg(ins, i)) {

	   REG r = INS_OperandReg(ins, i);
            opndvals[valcount].reg = r;
            opndvals[valcount].type = GetRegType(ins, i);

	    REG fullr = REG_FullRegName(r);
	    uint32_t fulltype = GetTypeOfReg(fullr);
	    if (fullr != REG_INVALID() && fullr != r && fulltype != VT_NONE) {
	      /* We know the name and type of the fuller register, so just use that! */
	      //	      cerr << "partial " << REG_StringShort(r) << " full " << REG_StringShort(fullr) << endl;
	      opndvals[valcount].reg = fullr;
	      opndvals[valcount].type = fulltype;
	    }

            valcount++;

         } else if (INS_OperandIsMemory(ins, i) ||
		    INS_OperandIsAddressGenerator(ins, i)) {


           /* Note: Compiled code sometimes uses LEA instructions for
            * arithmetic.  As such, we always want to treat reads of
            * these base/index registers as tainted. */

            REG basereg = INS_OperandMemoryBaseReg(ins, i);
            if (basereg != REG_INVALID()) {

               opndvals[valcount].reg = basereg;
               opndvals[valcount].type = GetTypeOfReg(basereg);

               if (TaintedIndices || INS_OperandIsAddressGenerator(ins, i))
                  opndvals[valcount].taint = RD;
               else
                  opndvals[valcount].taint = 0;

               valcount++;

            }

            REG idxreg = INS_OperandMemoryIndexReg(ins, i);
            if (idxreg != REG_INVALID()) {

               opndvals[valcount].reg = idxreg;
               opndvals[valcount].type = GetTypeOfReg(idxreg);

               if (TaintedIndices || INS_OperandIsAddressGenerator(ins, i))
                  opndvals[valcount].taint = RD;
               else
                  opndvals[valcount].taint = 0;

               valcount++;              

            }
         } 	   
      }

      bool memRead = INS_IsMemoryRead(ins);
      bool memRead2 = INS_HasMemoryRead2(ins);
      bool memWrite = INS_IsMemoryWrite(ins);

      // Value type of memory read.
      uint32_t memReadTy = VT_NONE;
      if (memRead) {
        switch (INS_MemoryReadSize(ins)) {
            case 1: memReadTy = VT_MEM8; break;
            case 2: memReadTy = VT_MEM16; break;
            case 4: memReadTy = VT_MEM32; break;
            case 8: memReadTy = VT_MEM64; break;
            case 16: memReadTy = VT_MEM128; break;
            case 32: memReadTy = VT_MEM256; break;
            default:
              cerr << "ERROR: Unsupported memory read size: " <<
                decstr(INS_MemoryReadSize(ins)) << "!" << endl;
              cerr << "Instruction: " << INS_Disassemble(ins) << endl;
              cerr << "Instruction category: " <<
                CATEGORY_StringShort(INS_Category(ins)) << endl;
              assert(false);
              return;
        }
      }
      // Value type of memory read.
      uint32_t memWriteTy = VT_NONE;
      if (memWrite) {
         switch (INS_MemoryReadSize(ins)) {
             case 1: memWriteTy = VT_MEM8; break;
             case 2: memWriteTy = VT_MEM16; break;
             case 4: memWriteTy = VT_MEM32; break;
             case 8: memWriteTy = VT_MEM64; break;
             case 16: memWriteTy = VT_MEM128; break;
             case 32: memWriteTy = VT_MEM256; break;
             default:
               cerr << "ERROR: Unsupported memory write size: " <<
                 decstr(INS_MemoryReadSize(ins)) << "!" << endl;
               cerr << "Instruction: " << INS_Disassemble(ins) << endl;
               cerr << "Instruction category: " <<
                 CATEGORY_StringShort(INS_Category(ins)) << endl;
               assert(false);
               return;
         }
      }
         
      // Insert the operand values we've previously identified into the arglist.
      for (unsigned int i = 0; i < valcount; i++) {

        // cerr << opndvals[i].type << " " << i << " " << valcount << endl;
        
        // LOG("Adding: " + REG_StringShort((REG)opndvals[i].reg) + "\n");

        /*
         * PIN has several ways of passing register values to analysis
         * functions.  Unfortunately, none of them works all the
         * time.  So, we need to decide how to pass the value, and set
         * the *_value arguments to AppendBuffer accordingly.
         */
        switch (howPass((REG) opndvals[i].reg)) {
            case P_CONTEXT:
              IARGLIST_AddArguments(arglist_helper,
                                    IARG_UINT32, opndvals[i].type,
                                    IARG_UINT32, opndvals[i].reg,
                                    /* We don't need the value
                                       argument for contexts */
                                    IARG_PTR, 0,
                                    IARG_UINT32, opndvals[i].taint,
                                    IARG_END);        
              break;

            case P_REF:
              IARGLIST_AddArguments(arglist_helper,
                                    IARG_UINT32, opndvals[i].type,
                                    IARG_UINT32, opndvals[i].reg,
                                    /* Pass reference pointer */
                                    IARG_REG_CONST_REFERENCE, opndvals[i].reg,
                                    IARG_UINT32, opndvals[i].taint,
                                    IARG_END);        
              break;
              
            default:
              cerr << "Unknown value passing method" << endl;
              assert(false);
        }
      }

      /* We break up memory operands into byte-wise operands.  This is
       * essential for taint analysis.  Code that utilizes taint
       * analysis assumes that a tainted value can be computed (e.g.,
       * symbolically executed) using the instructions in the trace.
       * However, if some of a memory operand are not tainted, then
       * they could have changed.  Thus, we must break up memory
       * operands to make this explicit. */
      
      if (memRead) {
        uint32_t bytes = GetByteSize(memReadTy);
        
        for (uint32_t offset = 0; offset < bytes; offset++) {
          IARGLIST_AddArguments(arglist_helper,
                                IARG_UINT32, VT_MEM8,
                                IARG_MEMORYREAD_EA,
                                //IARG_MEMORYREAD_SIZE,
                                IARG_UINT32, offset,
                                IARG_UINT32, RD,
                                IARG_END);
          valcount++;
        }
      }

      if (memRead2) {
        uint32_t bytes = GetByteSize(memReadTy);

        for (uint32_t offset = 0; offset < bytes; offset++) {        
          IARGLIST_AddArguments(arglist_helper,
                                IARG_UINT32, VT_MEM8,
                                IARG_MEMORYREAD2_EA,
                                //IARG_MEMORYREAD_SIZE,
                                IARG_UINT32, offset,
                                IARG_UINT32, RD,
                                IARG_END);
	      valcount++;
        }
      }

      if (memWrite) {
        uint32_t bytes = GetByteSize(memWriteTy);

        for (uint32_t offset = 0; offset < bytes; offset++) {        
          
          IARGLIST_AddArguments(arglist_helper,
                                IARG_UINT32, VT_MEM8,
                                IARG_MEMORYWRITE_EA,
                                //IARG_MEMORYWRITE_SIZE,
                                IARG_UINT32, offset,
                                IARG_UINT32, WR,
                                IARG_END);
	      valcount++;
        }
      }

      if (INS_SegmentPrefix(ins)) {
	REG seg = INS_SegmentRegPrefix(ins);
	/* Pin only has base registers for FS and GS (probably since
	   Linux uses GS, and Windows uses FS. So, we'll just output a
	   base register if we see one of those for now, and hope we
	   don't need ES/etc. */
	if (seg == REG_SEG_FS || seg == REG_SEG_GS) {
	  REG addreg;

	  /* Set the register to add to the buffer */
	  switch(seg) {
	  case REG_SEG_FS:
	    addreg = REG_SEG_FS_BASE;
	    break;
	    
	  case REG_SEG_GS:
	    addreg = REG_SEG_GS_BASE;
	    break;

	  default:
	    assert(false);
	    break;
	  }

	      IARGLIST_AddArguments(arglist_helper,
                                    IARG_UINT32, VT_REG32,
				    IARG_UINT32, addreg,
                                    //IARG_MEMORYWRITE_SIZE,
                                    IARG_PTR, 0,
                                    IARG_UINT32, 0,
                                    IARG_END);	  
	  valcount++;
	}
      }



      // TODO: Check if valcount has exceed the maximum number of
      // values. Also, figure out what to do if so.

      if (valcount >= MAX_VALUES_COUNT) {
	cerr << "Error: Too many values (" << valcount << "). Max: " << MAX_VALUES_COUNT << endl;
	cerr << "Instruction: " << INS_Disassemble(ins) << endl;
	cerr << "Category: " << CATEGORY_StringShort(INS_Category(ins)) << endl;
      }
      assert(valcount < MAX_VALUES_COUNT);
      

      IARGLIST_AddArguments(arglist,
		      IARG_UINT32, valcount,
		      IARG_END);

      /* Now, add the operands. */
      IARGLIST_AddArguments(arglist,
			    IARG_IARGLIST, arglist_helper,
			    IARG_END);

      // The argument list has been built, time to insert the call.

      INS_InsertCall(ins, IPOINT_BEFORE,
                     (AFUNPTR) AppendBuffer,
                     IARG_IARGLIST, arglist,
                     IARG_END);

      // If we are logging one instruction after exiting the recording
      // range, then arrange for the post instruction call to happen
      // if ins is outside of the range.
      if (LogOneAfter.Value() && !(INS_Address(ins) >= start_addr && INS_Address(ins) <= end_addr)) {
        cerr << "found the last one" << endl;
        if (INS_IsBranchOrCall(ins)) {
        INS_InsertCall(ins, IPOINT_TAKEN_BRANCH,
                       (AFUNPTR) PostInstruction,
                       IARG_ADDRINT, INS_Address(ins),
                       IARG_CONTEXT,
                       IARG_END);
        } else {
        INS_InsertCall(ins, IPOINT_AFTER,
                       (AFUNPTR) PostInstruction,
                       IARG_ADDRINT, INS_Address(ins),
                       IARG_CONTEXT,
                       IARG_END);
        }
      }
      
      insLeft--;

      // Free the memory.
      IARGLIST_Free(arglist);
      IARGLIST_Free(arglist_helper);

   }

   //LOG("INS: bbl ins end.\nINS: BBL end.\n");
   

}

VOID InstrTrace(TRACE trace, VOID *v)
{

  /* Decide if we want to log this trace by examining the entrance address. */
  ADDRINT addr = TRACE_Address(trace);
  if (dontLog(addr)) {
  } else {
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl)) {
      InstrBlock(bbl);
    }
  }
}

VOID ThreadEnd(THREADID threadid, CONTEXT *ctx, INT32 code, VOID *v)
{
  ThreadInfo_t *ti = NULL;
  
  cerr << "Thread " << threadid << " ending" << endl;

  // Free thread-local data
  ti = GetThreadInfo();
  
  delete ti;
}

VOID ThreadStart(THREADID threadid, CONTEXT *ctx, INT32 flags, VOID *v)
{
   // Get the command line arguments before _start is called
   // This only works with Linux conventions in mind
  static int firstthread = true;

  LLOG("new thread\n");
  
  NewThreadInfo();

  GetLock(&lock, threadid+1);
  
  LOG("New thread starting\n");
  cerr << "Thread " << threadid << " starting" << endl;

  if (firstthread) {
    firstthread = false;
#ifndef _WIN32 /* unix */
    int argc = *(int*)(PIN_GetContextReg(ctx, REG_ESP));
    char **argv = (char**) (PIN_GetContextReg(ctx, REG_ESP)+4);
    char **env = (char**) (PIN_GetContextReg(ctx, REG_ESP)+(argc+1)*4);
    std::vector<TaintFrame> frms = tracker->taintArgs(argc, argv);
    g_tw->add(frms);
    frms = tracker->taintEnv(env);
    g_tw->add(frms);
#else /* windows */
    /* On windows, we don't taint argc and argv, but rather taint the
       output of GetComamndLineA and GetCommandLineW.  On recent
       versions of Windows, these return a static pointer. */
    char *aptr = WINDOWS::GetCommandLineA();
    wchar_t *wptr = WINDOWS::GetCommandLineW();

    std::vector<TaintFrame> frms = tracker->taintArgs(aptr, wptr);
    g_tw->add(frms);
#endif
  }

  ReleaseLock(&lock);  

}

VOID ModLoad(IMG img, VOID *v)
{

  cerr << "This is modload()" << endl;

   LoadModuleFrame f;
   f.low_addr = IMG_LowAddress(img);
   f.high_addr = IMG_HighAddress(img);
   f.start_addr = IMG_StartAddress(img);
   f.load_offset = IMG_LoadOffset(img);

   const string &name = IMG_Name(img);

   size_t sz = name.size() < 64 ? name.size() : 63;

   memset(&(f.name), 0, 64);
   memcpy(&(f.name), name.c_str(), sz);

   g_tw->add(f);

#ifdef _WIN32
   // Try to find kernel32
   {
     char tempbuf[BUFSIZE];
     char *tok = NULL;
     char *lasttok = NULL;

     // Fill up the temporary buffer
     strncpy(tempbuf, name.c_str(), BUFSIZE);

     // We don't need a lock, since this is an instrumentation function (strtok is not re-entrant)
     strtok(tempbuf, "\\");

     while ((tok = strtok(NULL, "\\")) != NULL) {
       // Just keep parsing...
       lasttok = tok;
     }

     if (lasttok) {
       if (strncmp(windowsDll, lasttok, BUFSIZE) == 0) {

         /* GetEnvironmentStringsA uses a table-based conversion
          * process that we can't analyze very well. So, these are
          * disabled. */
#ifdef USE_GETENVSTRINGS
	 RTN r;
	       
	 /** The prototype for GetEnvironmentStrings[WA] */
	 PROTO proto = PROTO_Allocate( PIN_PARG(uint32_t), CALLINGSTD_STDCALL,
				       "Windows API",
				       PIN_PARG_END() );
	       
	 r = RTN_FindByName(img, "GetEnvironmentStringsW");
	 if (r != RTN_Invalid()) {				    
	   RTN_ReplaceSignature(r, AFUNPTR(GetEnvWWrap),
				IARG_PROTOTYPE, proto,
				IARG_CONTEXT,
				IARG_ORIG_FUNCPTR,
				IARG_THREAD_ID,
				IARG_END);
	 } else {
	   cerr << "Warning: Error instrumenting GetEnvironmentStringsW()" << endl;
	 }

	 r = RTN_FindByName(img, "GetEnvironmentStringsA");
	 if (r != RTN_Invalid()) {				    
	   RTN_ReplaceSignature(r, AFUNPTR(GetEnvAWrap),
				IARG_PROTOTYPE, proto,
				IARG_CONTEXT,
				IARG_ORIG_FUNCPTR,
				IARG_THREAD_ID,
				IARG_END);
	 } else {
	   cerr << "Warning: Error instrumenting GetEnvironmentStringsA()" << endl;
	 }

	       
	 PROTO_Free(proto);
#endif
	       
       } else if (strncmp(wsDll, lasttok, BUFSIZE) == 0) {
	 /* Winsock */
	 RTN r;

	 cerr << "found winsock" << endl;

	 r = RTN_FindByName(img, "accept");
	 if (r != RTN_Invalid()) {

	   PROTO proto = PROTO_Allocate(PIN_PARG(uint32_t), CALLINGSTD_STDCALL,
					"accept",
					PIN_PARG(uint32_t),
					PIN_PARG(void*),
					PIN_PARG(int*),
					PIN_PARG_END());

	   RTN_ReplaceSignature(r, AFUNPTR(AcceptWrapper),
				IARG_PROTOTYPE, proto,
				IARG_CONTEXT,
				IARG_ORIG_FUNCPTR,
				IARG_THREAD_ID,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
				IARG_END);


	   PROTO_Free(proto);
					
	 } else {
	   cerr << "Couldn't find accept" << endl;
	 }

	 r = RTN_FindByName(img, "connect");
	 if (r != RTN_Invalid()) {

	   PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
					"connect",
					PIN_PARG(uint32_t),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG_END());

	   RTN_ReplaceSignature(r, AFUNPTR(ConnectWrapper),
				IARG_PROTOTYPE, proto,
				IARG_CONTEXT,
				IARG_ORIG_FUNCPTR,
				IARG_THREAD_ID,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
				IARG_END);


	   PROTO_Free(proto);
					
	 } else {
	   cerr << "Couldn't find connect" << endl;
	 }

	 r = RTN_FindByName(img, "WSAConnect");
	 if (r != RTN_Invalid()) {

	   PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
					"WSAConnect",
					PIN_PARG(uint32_t),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG_END());

	   RTN_ReplaceSignature(r, AFUNPTR(WSAConnectWrapper),
				IARG_PROTOTYPE, proto,
				IARG_CONTEXT,
				IARG_ORIG_FUNCPTR,
				IARG_THREAD_ID,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 2,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 4,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 5,
				IARG_FUNCARG_ENTRYPOINT_VALUE, 6,
				IARG_END);


	   PROTO_Free(proto);
					
	 } else {
	   cerr << "Couldn't find WSAConnect" << endl;
	 }


	 r = RTN_FindByName(img, "recv");
	 if (r != RTN_Invalid()) {

	   PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
					"recv",
					PIN_PARG(uint32_t),
					PIN_PARG(char*),
					PIN_PARG(int),
					PIN_PARG(int),
					PIN_PARG_END());
	   RTN_Open(r);

	   RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)BeforeRecv,
			  IARG_PROTOTYPE, proto,
			  IARG_THREAD_ID,
			  IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
			  IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
			  IARG_END);

	   RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
			  IARG_PROTOTYPE, proto,
			  IARG_THREAD_ID,
			  IARG_FUNCRET_EXITPOINT_VALUE,
			  IARG_PTR, "recv",
			  IARG_END);

	   RTN_Close(r);
	   PROTO_Free(proto);

	 } else {
	   cerr << "Couldn't find recv" << endl;
	 }


	 r = RTN_FindByName(img, "recvfrom");
	 if (r != RTN_Invalid()) {

	   PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
					"recvfrom",
					PIN_PARG(uint32_t),
					PIN_PARG(char*),
					PIN_PARG(int),
					PIN_PARG(int),
					PIN_PARG(void*),
					PIN_PARG(int*),
                                        PIN_PARG_END());
#if 0
	   RTN_Open(r);

	   RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)BeforeRecv,
	   		  IARG_PROTOTYPE, proto,
	   		  IARG_THREAD_ID,
	   		  IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
	   		  IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
	   		  IARG_END);

	   RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
	   		  IARG_PROTOTYPE, proto,
	   		  IARG_THREAD_ID,
	   		  IARG_FUNCRET_EXITPOINT_VALUE,
			  IARG_PTR, "recvfrom",
	   		  IARG_END);

	   RTN_Close(r);
#endif
	   PROTO_Free(proto);

	 } else {
	   cerr << "Couldn't find recvfrom" << endl;
	 }         
         
	 r = RTN_FindByName(img, "WSARecv");
	 if (r != RTN_Invalid()) {

	   PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
					"WSARecv",
					PIN_PARG(uint32_t),
					PIN_PARG(void*),
					PIN_PARG(uint32_t),
					PIN_PARG(uint32_t*),
					PIN_PARG(uint32_t*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG_END());

	   RTN_Open(r);

	   RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)WSABeforeRecv,
			  IARG_PROTOTYPE, proto,
			  IARG_THREAD_ID,
			  IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
			  IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
			  IARG_FUNCARG_ENTRYPOINT_VALUE, 3,
			  IARG_END);

	   RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
			  IARG_PROTOTYPE, proto,
			  IARG_THREAD_ID,
			  IARG_FUNCRET_EXITPOINT_VALUE,
			  IARG_PTR, "WSARecv",
			  IARG_END);

	   RTN_Close(r);
	   PROTO_Free(proto);

	 } else {
	   cerr << "Couldn't find WSArecv" << endl;
	 }

	 r = RTN_FindByName(img, "WSARecvFrom");
	 if (r != RTN_Invalid()) {

	   PROTO proto = PROTO_Allocate(PIN_PARG(int), CALLINGSTD_STDCALL,
					"WSARecvFrom",
					PIN_PARG(uint32_t),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG(void*),
					PIN_PARG_END());

#if 0
	   RTN_Open(r);

	   RTN_InsertCall(r, IPOINT_BEFORE, (AFUNPTR)WSABeforeRecv,
			  IARG_PROTOTYPE, proto,
			  IARG_THREAD_ID,
			  IARG_FUNCARG_ENTRYPOINT_VALUE, 0,
			  IARG_FUNCARG_ENTRYPOINT_VALUE, 1,
			  IARG_END);

	   RTN_InsertCall(r, IPOINT_AFTER, (AFUNPTR)AfterRecv,
			  IARG_PROTOTYPE, proto,
			  IARG_THREAD_ID,
			  IARG_FUNCRET_EXITPOINT_VALUE,
			  IARG_PTR, "WSARecvFrom",
			  IARG_END);

	   RTN_Close(r);
#endif

	   PROTO_Free(proto);

	 } else {
	   cerr << "Couldn't find WSArecv" << endl;
	 }

       } else {
	 cerr << "Other img: " << lasttok << endl;
       }
     }
   }
#endif

   if (g_usetrigger && !g_trig_resolved) {
      // Check if this module can be used to resolve the trigger address.

      // If no trigger module is set, then we just use the first one we
      // find, i.e. the main module.
      if ((KnobTrigModule.Value() == "") ||
          name.find(KnobTrigModule.Value()) != string::npos) {
         // Found the module, resolve address.

         g_trig_addr = KnobTrigAddr.Value() + IMG_LoadOffset(img);
         g_trig_resolved = true;

      }

   }
   
}

VOID SyscallEntry(THREADID tid, CONTEXT *ctx, SYSCALL_STANDARD std, VOID *v)
{
  ThreadInfo_t *ti = NULL;
  SyscallInfo_t si;

  /*
   * Synchronization note: We assume there is only one system call per
   * thread, and thus the thread local syscall stack does not need any
   * locking.
   */
  
  // cerr << "syscall in " << PIN_GetSyscallNumber(ctx, std) << endl;

  ti = GetThreadInfo();
  //  cerr << "stack size " << ti->scStack.size() << endl;

  LLOG("syscall\n");

  // Ignore if not activated.
  //  if (!g_active) return;

  // Get the address from instruction pointer (should be EIP).
  si.sf.addr = (uint32_t) PIN_GetContextReg(ctx, REG_INST_PTR);
  
  si.sf.tid = tid;
  
  si.sf.callno = (uint32_t) PIN_GetSyscallNumber(ctx, std);
  
  for (int i = 0; i < MAX_SYSCALL_ARGS; i++)
  {
    if (i < PLAT_SYSCALL_ARGS) {
      si.sf.args[i] = 
        (uint32_t) PIN_GetSyscallArgument(ctx, std, i);
    } else {
        si.sf.args[i] = (uint32_t)NULL;
    }
  }

  // XXX: This should really be above g_active probably, but it seems
  // unlikely that it would cause a problem.  It's here because
  // FlushBuffer obtains a lock of it's own.
  GetLock(&lock, tid+1);

  // First we need to flush the buffer, so we can directly add the
  // syscall frame after the frame for the instruction that led to the
  // syscall.
  FlushBuffer(true, ctx, tid, false);

  if (LogAllSyscalls.Value()) {
    g_tw->add(si.sf);
  }
  
  if (tracker->taintPreSC(si.sf.callno, si.sf.args, si.state)) {
    // Do we need to do anything here? ...
  }
  
  ti->scStack.push(si);
  
  //e:

  LLOG("releasing sysenter\n");
  ReleaseLock(&lock);  
  LLOG("really done with sysenter\n");
}

VOID SyscallExit(THREADID tid, CONTEXT *ctx, SYSCALL_STANDARD std, VOID *v)
{
  ThreadInfo_t *ti = NULL;
  SyscallInfo_t si;
  uint32_t addr, length;

  /*
   * Synchronization note: We assume there is only one system call per
   * thread, and thus the thread local syscall stack does not need any
   * locking.
   */

  // Ignore if not activated.
  // if (!g_active) return;

  LLOG("sysexit\n");
 
  ti = GetThreadInfo();
 
  si = ti->scStack.top();
  ti->scStack.pop();

  GetLock(&lock, tid+1);
  
  // Check to see if we need to introduce tainted bytes as a result of this
  // sytem call
  std::vector<TaintFrame> tfs = tracker->taintPostSC(PIN_GetSyscallReturn(ctx, std), si.sf.args, addr, length, si.state);

  if (tfs.size() > 0) {
    if (!g_taint_introduced) {
      // Activate taint tracking
      TActivate();
    }
    g_tw->add(tfs);
  }

  //printf("syscall out %d\n", si.sf.callno);

  LLOG("releasing sysexit\n");
  ReleaseLock(&lock);  

  // Untaint system call output registers (uses thread-local delta, so no lock needed)
  tracker->postSysCall(ti->delta);

  LLOG("really done with sysexit\n");
}


VOID FollowParent(THREADID threadid, const CONTEXT* ctxt, VOID * arg)
{
  int i;

  LLOG("fparent\n");
  
  GetLock(&lock, threadid+1);
  i = strlen(g_threadname);
  assert(i < BUFFER_SIZE);
  g_threadname[i++] = 'p';

  std::cerr << "Spawning parent: " << PIN_GetPid() << g_threadname << std::endl;

  ReleaseLock(&lock);  
}
  
VOID ExceptionHandler(THREADID threadid, CONTEXT_CHANGE_REASON reason, const CONTEXT *from, CONTEXT *to, INT32 info, VOID *v) {
  /*
    CONTEXT_CHANGE_REASON_FATALSIGNAL 	 Receipt of fatal Unix signal.
    CONTEXT_CHANGE_REASON_SIGNAL 	 Receipt of handled Unix signal.
    CONTEXT_CHANGE_REASON_SIGRETURN 	 Return from Unix signal handler.
    CONTEXT_CHANGE_REASON_APC 	 Receipt of Windows APC.
    CONTEXT_CHANGE_REASON_EXCEPTION 	 Receipt of Windows exception.
    CONTEXT_CHANGE_REASON_CALLBACK 	 Receipt of Windows call-back.
  */

  /*
    If there is a fatal exception, we should halt the trace as soon
    as possible, so we can exit.
    
    Also, FlushInstructions() needs mutual exclusivity.
  */

  // SWHITMANXXX Get information and put it into exception frame here
  // XXX Put this into frame buffer

  ExceptionFrame ef;
  cerr << "context change" << endl;
  LLOG("Exception!\n");
  // Get the address from instruction pointer (should be EIP).
  if (from)
    ef.from_addr = (uint32_t) PIN_GetContextReg(from, REG_INST_PTR);
  else
    ef.from_addr = -1;

  // Fatal signals have no 'to' context
  if (to)
    ef.to_addr = (uint32_t) PIN_GetContextReg(to, REG_INST_PTR);
  else
    ef.to_addr = -1;
  ef.tid = threadid;
  ef.exception = info;

  GetLock(&lock, threadid+1);  
  LLOG("got except lock!\n");

  // If we want the exception to be the last thing in the trace when
  // we crash, then we need to flush.
  FlushBuffer(false, from, threadid, false);
  g_tw->add(ef);

  if (reason == CONTEXT_CHANGE_REASON_FATALSIGNAL) {
    std::cerr << "Received fatal signal " << info << endl;
    FlushBuffer(false, from, threadid, false);
    Cleanup();
    exit(1);
  } else if (reason == CONTEXT_CHANGE_REASON_EXCEPTION) {

#ifdef _WIN32
    ADDRINT pc = PIN_GetContextReg(from, REG_INST_PTR);
    cerr << "Received windows exception @" << pc << " " << info << " in thread " << threadid << endl;

    if (info == accessViolation && SEHMode.Value() && g_taint_introduced) {
      cerr << "SEH mode activated!" << endl;
      ADDRINT old_esp = PIN_GetContextReg(from, REG_STACK_PTR);
      ADDRINT new_esp = PIN_GetContextReg(to, REG_STACK_PTR);

      cerr << "old esp: " << old_esp << " new esp: " << new_esp
	   << " old eip: " << PIN_GetContextReg(from, REG_INST_PTR) << " new eip: " << PIN_GetContextReg(to, REG_INST_PTR) << endl;
      
      /* The windows exception handler just pushed a bunch of crap
	 onto the stack.  Although some of this is user controllable,
	 we can untaint it for now, since we are mainly concerned with
	 accessing our buffer. */
      assert (new_esp < old_esp);
      for (ADDRINT ptr = new_esp; ptr < old_esp; ptr++) {
	tracker->untaintMem(ptr);
      }

      /* Try to find a tainted exception handler. */
      ADDRINT eptr = PIN_GetContextReg(to, REG_SEG_FS_BASE) 
	+ ehandler_fs_offset;

      /* eptr points to the &(head of SEH). */
      assert(PIN_SafeCopy(&eptr, (void*)eptr, sizeof(ADDRINT)) == sizeof(ADDRINT));

      /* eptr points to head of SEH. */

      for (int i = 0; i < maxSehLength; i++) {
      // while (true) {
	struct {
	  ADDRINT nptr;
	  ADDRINT handler;
	} buf;
	/* We supposedly have a pointer to an exception handler
	   structure.  Let's make sure it's mapped. */
	size_t b = PIN_SafeCopy((void*)&buf, (void*)eptr, ehandler_size);
	if (b == ehandler_size) {

	/* Okay, we have an exception handler.  Let's see if the
	   pointer handler is tainted.  So, check if M[eptr+4] is
	   tainted. */
	ADDRINT hptr = eptr + ehandler_handler_offset;

	/* hptr holds the address of the handler. */
	  cerr << "SEH handler M[" << hptr 
	       << "] = " << buf.handler
	       << " (" << tracker->getMemTaint(hptr, VT_MEM8) 
	       << ")"
	       << endl;

	eptr = buf.nptr;
	
	} else { 
	  cerr << "Unable to read from " << eptr << endl;
	  break; 
	}

      }

      ADDRINT esp = PIN_GetContextReg(to, REG_STACK_PTR);
      
      /* The exception handling stuff will push a lot of data to the
	 stack, so take account for that here. */
      PIN_SetContextReg(to, REG_STACK_PTR, esp-ehandler_esp_offset);
      
      PIVOT_testpivot(ps, to, *tracker);
      
      FlushBuffer(false, from, threadid, false);
      Cleanup();
      exit(1);
      
    } else {
      cerr << "Ignoring exception!" << endl;
    }
#endif
  } else if (reason == CONTEXT_CHANGE_REASON_CALLBACK) {
#if 0
    cerr << "Received windows callback" << endl;
#endif
  
  } else {
    std::cerr << "Received other exception " << reason << endl;
  }

  LLOG("done handling exception\n");
  ReleaseLock(&lock);
}

VOID FollowChild(THREADID threadid, const CONTEXT* ctxt, VOID * arg)
{
  int i;

  LLOG("follow child\n");
  
  GetLock(&lock, threadid+1);
  i = strlen(g_threadname);
  assert(i < BUFFER_SIZE);
  g_threadname[i++] = 'c';

  g_tw = new TraceWriter((g_threadname + KnobOut.Value()).c_str());
  
  g_bufidx = 0;
  g_kfcount = 0;
  
  g_logcount = 0;
  g_loglimit = KnobLogLimit.Value();
  
  g_timer = clock();
  std::cerr << "Spawning child: " << PIN_GetPid() << g_threadname << std::endl;
  ReleaseLock(&lock);
  
}

bool FollowExec(CHILD_PROCESS cp, VOID *v) {
  bool follow = false;  
  int argc;
  const char * const * argv;
  
  CHILD_PROCESS_GetCommandLine(cp, &argc, &argv);
  assert (argc >= 0);
  cerr << "Exec: ";
  for (int i = 0; i < argc; i++) {
    cerr << argv[i] << " ";
  }
  cerr << endl;
  
  /* See if we should follow this */
  for (unsigned int i = 0; i < FollowProgs.NumberOfValues(); i++) {
    if (FollowProgs.Value(i) == argv[0]) {
      follow = true;
    }
  }


if (follow)
  cerr << "Following" << endl;
 else
   cerr << "Not following" << endl;

#ifndef _WIN32
/* If we're on Linux, this means we're about to call execv(), and
 * we're going to disappear! We had better write out our trace! */

cerr << "Flushing buffer before execv()" << endl;
FlushBuffer(false, NULL, PIN_ThreadId(), true);
Cleanup();
#endif

return follow;
}

VOID Fini(INT32 code, VOID *v)
{
  LOG("In Fini");
  Cleanup();
}

// Caller responsible for mutual exclusion
VOID Cleanup()
{
   // Build TOC array.

   LOG("Building TOC...\n");
   cerr << "There are " << g_toc.size() << " elements in the TOC" << endl;
   
   uint32_t *toc = new uint32_t[g_toc.size() + 1];

   toc[0] = g_toc.size();
   for(uint32_t i = 0; i < toc[0]; i++)
      toc[i+1] = g_toc[i];
   
   LOG("done.\n");
   LOG("Finalizing trace...\n");

   g_tw->finalize(toc);

   delete toc;

   LOG("done.\n");

   clock_t endtime = clock();

   LOG("Time taken: " + decstr((UINT64) (endtime - g_timer)));

}

INT32 Usage()
{
    cerr << endl << KNOB_BASE::StringKnobSummary() << endl;
    return -1;
}

int main(int argc, char *argv[])
{
  stringstream ss;
  
  cerr << hex;

  // A sanity check for AppendBuffer
  assert(sizeof(RPassType) == sizeof(ADDRINT));
  
  PIN_InitSymbols();

   if (PIN_Init(argc,argv))
      return Usage();

   InitLock(&lock);

   // Check if a trigger was specified.
   if (KnobTrigAddr.Value() != 0) {
      g_usetrigger = true;
      g_trig_resolved = false;

      // Set trigger countdown to initial value.
      g_trig_countdown = KnobTrigCount.Value();
      
   } else {
      g_usetrigger = false;
   }
   // Check if taint tracking is on
   if (KnobTaintTracking.Value()) {
      tracker = new TaintTracker(values);
      for (uint32_t i = 0 ; i < TaintedFiles.NumberOfValues() ; i++) {
	if (TaintedFiles.Value(i) != "") {
	  tracker->trackFile(TaintedFiles.Value(i));
	}
      }

      tracker->setTaintArgs(TaintedArgs);
      if (TaintedStdin)
         tracker->setTaintStdin();
      if (TaintedNetwork)
         tracker->setTaintNetwork();
      if (TaintedEnv.Value() != "")
         tracker->setTaintEnv(TaintedEnv.Value());
   }

   /* Get a key for thread info */
   tl_key = PIN_CreateThreadDataKey(NULL);
   assert(tl_key != -1);

   // We must activate taint tracking early if we have tainted args
   // or envs
   if ((TaintedEnv.Value() != "") || TaintedArgs.Value()) {
     g_taint_introduced = true;
   } else {
     g_taint_introduced = false;
   }

   // Determine whether logging is enabled.
   // If a trigger is specified, logging is never enabled, because
   // nothing is logged until the trigger point.  Otherwise, logging
   // is enabled if taint is introduced, or if logging before taint is enabled.
   if (g_usetrigger) {
     g_active = false;
   } else {
     if (g_taint_introduced || LogAllBeforeTaint.Value()) {
       g_active = true;
     } else {
       g_active = false;
     }
   }

   cerr << "Logging initially enabled: " << g_active << endl;

   /** Read pivot gadgets */
   if (PivotFile.Value() != "") {
     fstream f;
     pivot_set::iterator i;

     f.open(PivotFile.Value().c_str());
     if (!f.is_open()) {
       cerr << "Could not open pivot gadget file: " << PivotFile.Value() << endl;
       exit(1);
     }

     ps = PIVOT_parseinput(f);
     cerr << "Read " << ps.size() << " pivots" << endl;

     f.close();
   }

   IMG_AddInstrumentFunction(ModLoad, 0);
   TRACE_AddInstrumentFunction(InstrTrace, 0);
   PIN_AddThreadStartFunction(ThreadStart, 0);
   PIN_AddThreadFiniFunction((THREAD_FINI_CALLBACK)ThreadEnd, 0);
   
   PIN_AddContextChangeFunction(ExceptionHandler, 0);
   
#ifndef _WIN32
   PIN_AddForkFunction(FPOINT_AFTER_IN_CHILD, FollowChild, 0);
   PIN_AddForkFunction(FPOINT_AFTER_IN_PARENT, FollowParent, 0);
#endif
   PIN_AddFollowChildProcessFunction(FollowExec, 0);
   
   PIN_AddSyscallEntryFunction(SyscallEntry, 0);
   PIN_AddSyscallExitFunction(SyscallExit, 0);
   
   PIN_AddFiniFunction(Fini, 0);

   ss << PIN_GetPid() << "-" << KnobOut.Value();
   
   g_tw = new TraceWriter(ss.str().c_str());

   g_bufidx = 0;
   g_kfcount = 0;
   
   g_logcount = 0;
   g_loglimit = KnobLogLimit.Value();

   g_skipTaints = SkipTaints.Value();

   g_timer = clock();

   g_exit_next = false;
   
   start_addr = TaintStart.Value();
   end_addr = TaintEnd.Value();

   cerr << "Code cache limit is " << CODECACHE_CacheSizeLimit() << endl;
   assert(CODECACHE_ChangeCacheLimit(CacheLimit.Value()));

   LOG("Starting program\n");
   cerr << "Starting program" << endl;

   // Start the program, never returns
   PIN_StartProgram();

   return 0;

}
