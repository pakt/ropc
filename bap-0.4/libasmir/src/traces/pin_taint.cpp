#include "pin.H"
#include "pin_taint.h"
#include "pin_frame.h"
#include "pin_syscalls.h"
#include "winsyscalls.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#include <cassert>
#include <sstream>

#ifdef _WIN32
/**
 * Getting WDK header files to include is a nightmare.  If you need to
 * change this, talk to me.
 *
 * -ejs
 */
namespace WINDOWS {
  // Define a target architecture for WDK
#define _X86_ 
#include "Wdm.h"
#undef _X86_
}

// Needed for STATUS_SUCCESS to work
typedef WINDOWS::NTSTATUS NTSTATUS;
#else
const unsigned int UNIX_SUCCESS = 0;
const unsigned int UNIX_FAILURE = -1;
#endif

using namespace std;
using namespace pintrace;

/** Skip this many taint introductions. */
int g_skipTaints = 0;

/** Reuse taint ids? */
const bool reuse_taintids = true;

/**************** Helper **********************/

/** Convert a wide string to a narrow one
 */
auto_ptr<string> GetNarrowOfWide(wchar_t *in) {
  /* Our output */
  //  string *out = new string;
  auto_ptr<string> out (new string);

  for (unsigned int i = 0; i < wcslen(in); i++) {
    out->push_back(
      use_facet<ctype<wchar_t> >(std::locale("")).narrow(in[i], '?')
		   );
  }

  return out;
}

/** Default Taint policy function */
bool defaultPolicy(uint32_t addr, uint32_t length, const char *msg) {
  static int intronum = -1;

  intronum++;

  cerr << "Taint introduction #" << intronum
       << ". @" << addr << "/" << length << " bytes: "
       << msg << endl;

  if (intronum >= g_skipTaints) {
    return true;
  } else {
    cerr << "Skipping taint introduction." << endl;
    return false;
  }
}

/**************** Initializers ****************/

//
TaintTracker::TaintTracker(ValSpecRec * env) 
  : taintnum(1),
    values(env),
    taint_net(false),
    taint_args(false),
    pf(defaultPolicy)
{}

//
void TaintTracker::setCount(uint32_t cnt)
{
  count = cnt;
}

//
void TaintTracker::setTaintArgs(bool taint)
{
  taint_args = taint;
}

//
void TaintTracker::setTaintEnv(string env_var)
{
  taint_env.insert(env_var);
}

//
void TaintTracker::trackFile(string file)
{
  taint_files.insert(file);
}

//
void TaintTracker::setTaintStdin()
{
#ifndef _WIN32
  fdInfo_t fd(string("stdin"), 0);
  fds[STDIN_FILENO] = fd;
#else
  assert(FALSE);
#endif
}

//
void TaintTracker::setTaintNetwork()
{
  taint_net = true;
}

/**************** Helper Functions ****************/

//
bool TaintTracker::isValid(uint32_t type)
{
  return (type != VT_NONE);
}

// 
bool TaintTracker::isReg(uint32_t type)
{
  return isValid(type) && (type >= REG_BASE) && (type <= REGTYPE_LAST);
}

bool TaintTracker::isMem(uint32_t type)
{
  return isValid(type) && (type >= MEM_BASE) && (type <= MEMTYPE_LAST);
}

// 
uint32_t TaintTracker::exists(context &ctx, uint32_t elem)
{
  return (ctx.find(elem) != ctx.end());
}

//
uint32_t TaintTracker::getSize(uint32_t type)
{
  uint32_t size;
  switch (type) {
      case VT_MEM128: case VT_REG128: size = 16; break;
      case VT_MEM64: case VT_REG64: size = 8; break;
      case VT_MEM32: case VT_REG32: size = 4; break;
      case VT_MEM16: case VT_REG16: size = 2; break;
      case VT_MEM8:  case VT_REG8:  size = 1; break;
      default:                      assert(false);
  }
  return size;
}

// Combining two taint tags
uint32_t TaintTracker::combineTaint(uint32_t oldtag, uint32_t newtag)
{
  if (newtag) {// its tainted
    if (oldtag == NOTAINT)
      return newtag; // FIXME
    else 
      return MIXED_TAINT;
  }
  return oldtag;
}

// 
void TaintTracker::printRegs(context &delta)
{
  cerr << hex << endl << " ----------- Tainted Regs ------------ " << endl;
  for (context::iterator it = delta.begin(), ie = delta.end() ; it != ie ; ++it)
       cerr << REG_StringShort((REG)it->first) << " = " << it->second << endl;
}

//
void TaintTracker::printMem()
{
  cerr << hex << endl << " ----------- Tainted Mem ------------ " << endl;
  for (context::iterator it = memory.begin(), ie = memory.end() ; it != ie ; ++it)
    cerr << "Addr: " << it->first << " -> " << it->second << endl;
}

/***************** Taint Handlers *******************/

// Reads length bytes from source at offset, putting the bytes at
// addr. If offset is -1, new tainted bytes are assigned. Otherwise,
// the (source,offset) tuple are compared for each byte to see if that
// resource has been used before, and if so, the same taint number is given.
std::vector<TaintFrame> TaintTracker::introMemTaint(uint32_t addr, uint32_t length, const char *source, int64_t offset) {

  std::vector<TaintFrame> tfs;
  
  if ((*pf)(addr, length, source)) {

    for (unsigned int i = 0; i < length; i++) {
      uint32_t t = 0;
      if (offset == -1 || reuse_taintids == false) {
        t = taintnum++;
      } else {
        // Check if (source, offset+i) has a byte. If not, assign one.
        resource_t r(source, offset+i);
        if (taint_mappings.find(r) != taint_mappings.end()) {
          t = taint_mappings[r];
          cerr << "found mapping from " << source << " to " << offset+i << " on taint num " << t << endl;
        } else {
          t = taintnum++;
          taint_mappings[r] = t;
          cerr << "adding new mapping from " << source << " to " << offset+i << " on taint num " << t << endl;
        }
      }
      /* Mark memory as tainted */
      setTaint(memory, addr+i, t);
      TaintFrame tf;
      tf.id = t;
      tf.length = 1;
      tf.addr = addr+i;
      tfs.push_back(tf);
    }
    return tfs;
  } else {
    return tfs;
  }
}

// Reads length bytes from source at offset, putting the bytes at
// addr. Also adds length to the offset of the resource.
std::vector<TaintFrame> TaintTracker::introMemTaintFromFd(uint32_t fd, uint32_t addr, uint32_t length) {
  assert(fds.find(fd) != fds.end());
  std::vector<TaintFrame> tfs = introMemTaint(addr, length, fds[fd].name.c_str(), fds[fd].offset);
  fds[fd].offset += length;
  return tfs;
}

//
void TaintTracker::setTaint(context &ctx, uint32_t key, uint32_t tag)
{
  if (tag == NOTAINT)
    ctx.erase(key);
  else ctx[key] = tag;
}


// 
uint32_t TaintTracker::getTaint(context &ctx, uint32_t elem)
{
  if (exists(ctx, elem))
    return ctx[elem];
  return NOTAINT;
}

// 
uint32_t TaintTracker::getMemTaint(uint32_t addr, uint32_t type)
{
  uint32_t tag = NOTAINT;
  //cerr << "Getting memory " << addr << endl;
  uint32_t size = getSize(type);
  for (uint32_t i = 0 ; i < size ; i++) {
    uint32_t status = getTaint(memory, addr+i);
    tag = combineTaint(tag, status);
  }
  return tag;
}

void TaintTracker::untaintMem(uint32_t addr) {
  setTaint(memory, addr, NOTAINT);
}

// 
uint32_t TaintTracker::getRegTaint(context &delta, uint32_t reg)
{
  // cout << "Partial register: " << REG_StringShort((REG)reg) << endl;
  REG temp = REG_FullRegName((REG)reg);
  // cerr << "Full register: " << REG_StringShort(temp) << endl;
  return getTaint(delta,temp);
}

// 
uint32_t TaintTracker::getReadTaint(context &delta)
{
  uint32_t tag = NOTAINT, tmp_tag = NOTAINT;
  for (uint32_t i = 0 ; i < count ; i++) {
    if ((values[i].usage & RD) == RD) {
      // this is a read
      if (isReg(values[i].type) 
	  && (values[i].loc != REG_EFLAGS)) // FIXME: no control-flow taint
	tmp_tag = getRegTaint(delta, values[i].loc);
      else if (isMem(values[i].type))
	tmp_tag = getMemTaint(values[i].loc, values[i].type);
      tag = combineTaint(tag, tmp_tag);
    }
  }
  return tag;
}

/************* External Taint Hooks **************/

/** Called after a system call to untaint the output register */
void TaintTracker::postSysCall(context &delta) {

  /* Windows uses EDX, and Linux uses EAX */

#ifdef _WIN32
  setTaint(delta, SCOUTREG_WIN, NOTAINT);
#else /* linux */
  setTaint(delta, SCOUTREG_LIN, NOTAINT);
#endif
}

void TaintTracker::acceptHelper(uint32_t fd) {
  if (taint_net) {
    cerr << "Tainting fd " << fd << endl;
    fdInfo_t fdinfo(string("accept"), 0);
    fds[fd] = fdinfo;
  }
}

std::vector<TaintFrame> TaintTracker::recvHelper(uint32_t fd, void *ptr, size_t len) {
  uint32_t addr = reinterpret_cast<uint32_t> (ptr);

  if (fds.find(fd) != fds.end()) {

    cerr << "Tainting " << len << " bytes of recv @" << addr << endl;
    return introMemTaintFromFd(fd, addr, len);
  } else {
    std::vector<TaintFrame> tfs;
    return tfs;
  }
}

/******************* Taint Analysis Rules ***************/

/******** Taint Introduction **********/

//
#ifdef _WIN32
std::vector<TaintFrame> TaintTracker::taintArgs(char *cmdA, wchar_t *cmdW)
{
  std::vector<TaintFrame> frms;
  std::vector<TaintFrame> tfrms;
  if (taint_args) {
    size_t lenA = strlen(cmdA);
    size_t lenW = wcslen(cmdW);
    size_t bytesA = lenA*sizeof(char);
    size_t bytesW = lenW*sizeof(wchar_t);
    cerr << "Tainting multibyte command-line arguments: " << bytesA << " bytes @ " << (unsigned int)(cmdA) << endl;
    
    /* Taint multibyte command line */
    frms = introMemTaint((uint32_t)cmdA, bytesA, "Tainted Arguments", -1);

    cerr << "Tainting wide command-line arguments: " << bytesW << " bytes @ " << (unsigned int)(cmdW) << endl;
    tfrms = introMemTaint((uint32_t)cmdW, bytesW, "Tainted Arguments", -1);
    frms.insert(frms.end(), tfrms.begin(), tfrms.end());
  }
  return frms;
}
#else
std::vector<TaintFrame> TaintTracker::taintArgs(int argc, char **argv)
{
  if (taint_args) {
    cerr << "Tainting command-line arguments" << endl;
    for ( int i = 1 ; i < argc ; i++ ) {
		cerr << "Tainting " << argv[i] << endl;
      size_t len = strlen(argv[i]);
      return introMemTaint((uint32_t)argv[i], len, "Arguments", -1);      
    }
  }
  
  // XXX: Is this even safe?
  return std::vector<TaintFrame> ();
}
#endif

//
#ifdef _WIN32
std::vector<TaintFrame> TaintTracker::taintEnv(char *env, wchar_t *wenv)
{
  /* See MSDN docs here: http://msdn.microsoft.com/en-us/library/ms683187(VS.85).aspx 
   * Basically, env is a pointer to
   * var=val\x00
   * var2=val2\x00
   * ...
   * \x00\x00
   */
  //  std::vector<TaintFrame> frms;

  // /* Multibyte strings */
  // for ( ; *env != '\x00'; env += (strlen(env) + 1 /* null */)) {
  //   string var(env);
  //   int equal = var.find('=');
  //   var = var.substr(0, equal);
  //   if (taint_env.find(var) != taint_env.end()) {
  //     uint32_t len = strlen(env) - var.size();
  //     uint32_t addr = (uint32_t)env+equal+1;
  //     cerr << "Tainting environment variable: " << var << " @" << (int)addr << " " << len << " bytes" << endl;
  //     for (uint32_t j = 0 ; j < len ; j++) {
  // 	setTaint(memory, (addr+j), taintnum++);
  //     }
  //     TaintFrame frm;
  //     frm.id = ENV_ID;
  //     frm.addr = addr;
  //     frm.length = len;
  //     frms.push_back(frm);
  //   }
  // }

  /* Wide strings */
  if (wenv) {
    for ( ; *wenv != '\x00'; wenv += (wcslen(wenv) + 1 /* null */)) {
      string ns = *GetNarrowOfWide(wenv);
      wstring wvar(wenv);
      string var(ns);
      int equal = var.find('=');
      var = var.substr(0, equal);
      
      if (taint_env.find(var) != taint_env.end()) {
        uint32_t numChars = wcslen(wenv) - var.size();
	uint32_t numBytes = numChars * sizeof(wchar_t);
        uint32_t addr = (uint32_t) (wenv+equal+1);
        cerr << "Tainting environment variable: " << var << " @" << (int)addr << " " << numChars << " bytes" << endl;
	return introMemTaint(addr, numBytes, "Environment Variable", -1);

      }
    }
  }


  return std::vector<TaintFrame> ();
}
#else /* unix */
std::vector<TaintFrame> TaintTracker::taintEnv(char **env)
{
  std::vector<TaintFrame> frms;
  for ( int i = 1 ; env[i] ; i++ ) {
    string var(env[i]);
    int equal = var.find('=');
    var = var.substr(0,equal);
    if (taint_env.find(var) != taint_env.end()) {
      uint32_t len = strlen(env[i]) - var.size();
      uint32_t addr = (uint32_t)env[i]+equal+1;
      cerr << "Tainting environment variable: " << var << " @" << (int)addr << endl;
      return introMemTaint(addr, len, "environment variable", -1);
    }
  }
  return std::vector<TaintFrame> ();
}
#endif

/** This function is called right before a system call. */
bool TaintTracker::taintPreSC(uint32_t callno, uint32_t *args, /* out */ uint32_t &state)
{
  //cout << "Syscall no: " << callno << endl << "Args:" ;
  //for ( int i = 0 ; i < MAX_SYSCALL_ARGS ; i ++ )
  //  cout << hex << " " << args[i] ;
  //cout << endl ;
  state = __NR_nosyscall;
  
  bool reading_tainted = false;
  char filename[128];
  
  switch (callno) {
#ifndef _WIN32 /* unix */
      case __NR_open:
        // FIXME: use PIN_SafeCopy
        strncpy(filename, (char *)args[0],128); 
        if (taint_files.find(string(filename)) != taint_files.end()) {
          cerr << "Opening tainted file: " << string(filename) << endl;
          state = __NR_open;
        }
        break;
      case __NR_close:
        state = __NR_close;
        break;
        // TODO: do we care about the offset?
      case __NR_mmap:
      case __NR_mmap2:
        if (fds.find(args[4]) != fds.end()) {
          cerr << "mmapping " << args[0] << endl;
          state = __NR_mmap2;
        }
        break;
      case __NR_read: 
        if (fds.find(args[0]) != fds.end()) {
          state = __NR_read;
          reading_tainted = true;
        }
        break;
      case __NR_socketcall:
        // TODO: do we need to distinguish between sockets?
        if (taint_net) {
          state = __NR_socketcall;
          if (args[0] == _A1_recv)
            reading_tainted = true;
        }
        break;
      case __NR_execve:
        break;
      case __NR_lseek:
        if (fds.find(args[0]) != fds.end()) {
          state = __NR_lseek;
        }
        break;
        
#else /* windows */
  case __NR_createfilewin:
    {
      char tempstr[BUFSIZE];
      WINDOWS::POBJECT_ATTRIBUTES pattr;
      WINDOWS::PWSTR fname;
      size_t origsize;
      size_t convertedChars;
      
      pattr = reinterpret_cast<WINDOWS::POBJECT_ATTRIBUTES> (args[2]);
      assert(pattr);
      assert(pattr->ObjectName);
      fname = pattr->ObjectName->Buffer;
      origsize = wcslen(fname) + 1;
      convertedChars = 0;
      wcstombs_s(&convertedChars, tempstr, origsize, fname, BUFSIZE-1);
      if (convertedChars < origsize) {
	cerr << "Warning: Could not convert all characters" << endl;
      }
      
      if (taint_files.find(string(tempstr)) != taint_files.end()) {
	cerr << "Opening tainted file: " << string(tempstr) << endl;
	state = __NR_createfilewin;
      } else {
	cerr << "Not opening " << string(tempstr) << endl;
      }
      
      break;
    }
  case __NR_readfilewin:        
      if (fds.find(args[0]) != fds.end()) {
	reading_tainted = true;
	cerr << "found a t-read " << "len " << args[6] << " off " << args[7] << endl;
	state = __NR_readfilewin;
      }
      break;    
  case __NR_setinfofilewin:
    if (fds.find(args[0]) != fds.end()) {
      state = __NR_setinfofilewin;
    } 
    break;
  case __NR_closewin:
    if (fds.find(args[0]) != fds.end()) {
	state = __NR_closewin;
      }
    break;

  case __NR_createsectionwin:
    if (args[6]) {
      state = __NR_createsectionwin;
    }
    // pattr = reinterpret_cast<WINDOWS::POBJECT_ATTRIBUTES> (args[2]);
    // if (pattr) {
    //   assert(pattr);
    //   assert(pattr->ObjectName);
    //   fname = pattr->ObjectName->Buffer;
    //   origsize = wcslen(fname) + 1;
    //   convertedChars = 0;
    //   wcstombs_s(&convertedChars, tempstr, origsize, fname, BUFSIZE-1);
    //   if (convertedChars < origsize) {
    // 	cerr << "Warning: Could not convert all characters" << endl;
    //   }
      
    //   if (taint_files.find(string(tempstr)) != taint_files.end()) {
    // 	cerr << "Opening tainted file: " << string(tempstr) << endl;
    // 	state = __NR_createsectionwin;
    //   } else {
    // 	cerr << "Not opening " << string(tempstr) << endl;
    //   }
    // }
    break;
  case __NR_mapviewofsectionwin:
    if (sections.find(args[0]) != sections.end()) {
      reading_tainted = true;
      cerr << "found a t-mmap " << endl;
      state = __NR_mapviewofsectionwin;
      }
    break;
#endif
    
  default:
    //    LOG(string("Unknown system call") + *(get_name(callno)) + string("\n"));
    // cerr << "Unknown system call " << *(get_name(callno)) << endl;
    break;
  }
  return reading_tainted;
}

 /** This function is called immediately following a system call. */
std::vector<TaintFrame> TaintTracker::taintPostSC(const uint32_t bytes, 
                                     uint32_t *args,
                                     uint32_t &addr,
                                     uint32_t &length,
				     const uint32_t state)
{
  //for ( int i = 0 ; i < MAX_SYSCALL_ARGS ; i ++ )
  //cout << hex << " " << args[i] ;
  //cout << endl ;

  uint32_t fd = -1;
  
  switch (state) {
#ifndef _WIN32 /* unix */
      case __NR_socketcall:
        switch (args[0]) {
            case _A1_recv:
              addr = ((uint32_t *)args[1])[1];
              fd = args[0];
              length = bytes;
              cerr << "Tainting " 
                   << bytes 
                   << " bytes from socket" << endl;
              return introMemTaintFromFd(fd, addr, length);
              //return true;
      
            case _A1_accept:
              if (bytes != (uint32_t)UNIX_FAILURE) {
                cerr << "Accepting an incoming connection" << endl;
                fdInfo_t fdinfo(string("accept"), 0);
                fds[bytes] = fdinfo;
              }
              break;
            case _A1_socket:
              if (bytes != (uint32_t)UNIX_FAILURE) {
                cerr << "Opening a tainted socket " << bytes << endl;
                fdInfo_t fdinfo(string("socket"), 0);
                fds[bytes] = fdinfo;
              }
              break;
            default:
              break;
        }
        break;
      case __NR_open:
        // "bytes" contains the file descriptor
        if (bytes != (uint32_t)(UNIX_FAILURE)) { /* -1 == error */
          /* args[0] is filename */
          const char *filename = reinterpret_cast<const char *> (args[0]);
          fdInfo_t fdinfo(string("file ") + string(filename), 0);
          fds[bytes] = fdinfo;
        }
        break;
      case __NR_close:
        if (bytes == (uint32_t)(UNIX_SUCCESS) && fds.find(args[0]) != fds.end()) {
          cerr << "closed tainted fd " << args[0] << endl;
          fds.erase(args[0]);
        }
        break;
      case __NR_mmap:
      case __NR_mmap2:
      {
        addr = bytes;
        fd = args[5];
        length = args[1];
        uint32_t offset = args[6];
        cerr << "Tainting " 
             << length 
             << "bytes from mmap" << endl;
        return introMemTaint(addr, length, fds[fd].name.c_str(), (int64_t)offset);
        break;
      }
      case __NR_read:
      {
        fd = args[0];
        addr = args[1];
        length = bytes;
        cerr << "Tainting " 
             << length 
             << " bytes from read at " << addr
             << endl;

        return introMemTaintFromFd(fd, addr, length);
      }
      case __NR_lseek:
        if (bytes != UNIX_FAILURE) {
          cerr << "Changing offset for fd " << args[0] << " to " << bytes << endl;
          fds[args[0]].offset = bytes;
        } else {
          cerr << "lseek() failure!" << endl;
        }
        break;
#else /* windows */
      case __NR_createfilewin:
    
        // If opened
        if (bytes == STATUS_SUCCESS) {
          WINDOWS::PHANDLE p = reinterpret_cast<WINDOWS::PHANDLE> (args[0]);
          uint32_t fd = reinterpret_cast<uint32_t> (*p);
	  char tempstr[BUFSIZE];
	  WINDOWS::POBJECT_ATTRIBUTES pattr;
	  //errno_t e;
	  WINDOWS::PWSTR fname;
	  size_t origsize;
	  size_t convertedChars;
          cerr << "Tainting file descriptor " << fd << endl;

	  pattr = reinterpret_cast<WINDOWS::POBJECT_ATTRIBUTES> (args[2]);
	  assert(pattr);
	  assert(pattr->ObjectName);
	  fname = pattr->ObjectName->Buffer;
	  origsize = wcslen(fname) + 1;
	  convertedChars = 0;
	  wcstombs_s(&convertedChars, tempstr, origsize, fname, BUFSIZE-1);
	  if (convertedChars < origsize) {
	    cerr << "Warning: Could not convert all characters" << endl;
	  }

	  string fnamestr = string("file ") + string(tempstr);

	  cerr << "opened file " << fnamestr << endl;
	  fdInfo_t fdi(fnamestr, 0);
	  fds[fd] = fdi;
        }
        break;
      case __NR_readfilewin:
        if (bytes == STATUS_SUCCESS) {
          WINDOWS::PIO_STATUS_BLOCK psb = reinterpret_cast<WINDOWS::PIO_STATUS_BLOCK> (args[4]);
          assert(psb);
          assert(psb->Information);
          length = psb->Information;
          addr = args[5];
          assert(addr);
          cerr << "Tainting " 
               << length 
               << " bytes from read @" << addr << endl;
          return introMemTaintFromFd(args[0], addr, length);
          //return true;
        }
  case __NR_setinfofilewin:
    if (bytes == STATUS_SUCCESS) {
      uint32_t c = args[4];
      uint32_t fd = args[0];
      if (c == WINDOWS::FilePositionInformation) {
	/* Someone is setting the file position. */
	WINDOWS::PFILE_POSITION_INFORMATION pi = (WINDOWS::PFILE_POSITION_INFORMATION)(args[2]);
	fds[fd].offset = pi->CurrentByteOffset.QuadPart;
	cerr << "updated offset to " << fds[fd].offset << endl;
      }
    }
    break;
      case __NR_closewin:
        if (bytes == STATUS_SUCCESS) {
          cerr << "closed tainted fd " << args[0] << endl;
          fds.erase(args[0]);
        }
        break;
      case __NR_createsectionwin:
      {
        if (bytes == STATUS_SUCCESS) {
          WINDOWS::PHANDLE file = reinterpret_cast<WINDOWS::PHANDLE> (args[6]);
          uint32_t fdn = reinterpret_cast<uint32_t> (file);
          if (fds.find(fdn) != fds.end()) {
            WINDOWS::PHANDLE p = reinterpret_cast<WINDOWS::PHANDLE> (args[0]);
            uint32_t section = reinterpret_cast<uint32_t> (*p);
            cerr << "Created a section with the file handle: " << fdn << endl;
            sections[section] = fdn ;
          }
        }
        break;
      }
      case __NR_mapviewofsectionwin:
        if (bytes == STATUS_SUCCESS) {
	  /* XXX: Determine offset into file. */
          length = 0; // disabled args[6];  /// XXX: possibly wrong
          addr = *(reinterpret_cast<uint32_t *> (args[2])); /// XXX: possibly wrong
          assert(addr);
          cerr << "Tainting " 
               << length 
               << " bytes from read @" << addr << endl;
          return introMemTaint(addr, length, "read", -1);
          //return true;
        }
        break;
#endif
      default:
        break;
  }
  std::vector<TaintFrame> tfs;
  return tfs;
}

/******** Taint Propagation **********/

// Set taint of the current values based on taint context information
void TaintTracker::setTaintContext(context &delta)
{
  uint32_t tag;
  for (uint32_t i = 0 ; i < count ; i++) {
    if (isReg(values[i].type)) {
      if ((tag = getRegTaint(delta, values[i].loc)) != NOTAINT) {
	// cerr << "register: " << REG_StringShort((REG)values[i].loc) << " is tainted" << endl;
	values[i].taint = tag;
      }
    } else if (isValid(values[i].type)) {
      if ((tag = getTaint(memory,values[i].loc)) != NOTAINT) {
	//cerr << "memory: " << values[i].loc << " is tainted" << endl;
	values[i].taint = tag;
      }
    }
  }
  
}

// Reset the taint status of registers and memory
void TaintTracker::resetTaint(context &delta) {
  delta.clear();
  memory.clear();
}

// Add taint 'tag' to all written operands
void TaintTracker::addTaintToWritten(context &delta, uint32_t tag)
{
  uint32_t loc;
  cerr <<hex ;
  for (uint32_t i = 0 ; i < count ; i++) {
    if ((values[i].usage & WR) == WR)  {
      if (isReg(values[i].type)) {
	loc = REG_FullRegName((REG)values[i].loc);
	setTaint(delta,loc,tag);
	values[i].taint = getRegTaint(delta, loc);
	//cerr << "new " << REG_StringShort((REG)values[i].loc) 
	//     << " taint: " << values[i].taint << endl;
      } else if (isMem(values[i].type)) {
	//cerr << hex << "writing " << values[i].loc << " = " << tag << endl;
	loc = values[i].loc;
	uint32_t size = getSize(values[i].type);
	for(uint32_t j = 0 ; j < size ; j++) {
	  //cerr << " Tainting memory " << loc + j << endl;
	  setTaint(memory,loc+j,tag);
	}
	values[i].taint = getTaint(memory,loc);
	//cerr << "mem taint: " << values[i].taint << endl;
      } 
    }
  }
}

// Propagate taint information to written operands
void TaintTracker::taintPropagation(context &delta)
{
  //printMem();
  //printRegs();
  uint32_t taint_tag = getReadTaint(delta);
  addTaintToWritten(delta, taint_tag);
}

/******** Taint Checking **********/

// Check if the current instruction has tainted operands
bool TaintTracker::hasTaint(context &delta)
{
  cerr << hex ;
  for (uint32_t i = 0 ; i < count ; i++) {
    if (isReg(values[i].type)) {
      if (getRegTaint(delta, values[i].loc) != NOTAINT) {
	//cerr << "Tainted: " << REG_StringShort((REG)values[i].loc) << endl;
	return true;
      }
    } else if (isValid(values[i].type)) {
      if (getTaint(memory,values[i].loc) != NOTAINT) {
	//cerr << "Tainted Memory: " << values[i].loc << endl;
	return true;
      }
    }
  }
  return false;
}

// 
// bool TaintTracker::propagatedTaint(bool branch)
// {
//   if (branch)
//     return false;
//   for (uint32_t i = 0 ; i < count ; i++)
//     if ((values[i].usage == RD)
//         && isReg(values[i].type)
//         && values[i].loc != REG_EFLAGS
//         && values[i].taint != NOTAINT)
//       return true;
//   return false;
// } 

// Check of EIP is tainted
bool TaintTracker::taintChecking()
{
  for (uint32_t i = 0 ; i < count ; i++)
    if ((values[i].loc == REG_INST_PTR)
        && (isReg(values[i].type))
        && (values[i].taint != NOTAINT)) {
      return false;
    }
  return true;
}
