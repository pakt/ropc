#ifndef _PIN_SYSCALLS_H
#define _PIN_SYSCALLS_H

/* Misc. definitions that are related to system calls */

/***************** Syscalls ***************/
// FIXME: use the ones from /usr/include/asm/unistd.h

#define __NR_nosyscall            0     
#define __NR_read		  3
#define __NR_open		  5
#define __NR_close		  6
#define __NR_execve		 11
#define __NR_lseek               19
#define __NR_mmap		 90
#define __NR_socketcall	102
#define __NR_mmap2		192

// Windows system calls @ http://code.google.com/p/miscellaneouz/source/browse/trunk/winsyscalls?spec=svn26&r=26
// FIXME: We should really handle different versions of Windows better
// These are for win7
#define __NR_closewin            0x0032
#define __NR_createfilewin	 0x0042
#define __NR_createsectionwin    0x0054
#define __NR_mapviewofsectionwin 0x00A8
#define __NR_readfilewin         0x0111
#define __NR_setinfofilewin      0x0149

/********************************************/

// socket specific calls
#define _A1_socket     0x1
#define _A1_bind       0x2
#define _A1_listen     0x4
#define _A1_accept     0x5
#define _A1_send       0x9
#define _A1_recv       0xa
#define _A1_setsockopt 0xe

/********************************************/

/*************** Syscall Regs ***************/
#define SCOUTREG_WIN REG_EDX
#define SCOUTREG_LIN REG_EAX

#endif
