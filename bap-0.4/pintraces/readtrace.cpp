#include <iostream>
#include <string>
#include <cstdio>

#include "pin_trace.h"

using namespace std;
using namespace pintrace;

int main(int argc, char **argv) {

   pintrace::TraceReader tr(argv[1]);

   printf("Frame count: %d\n", tr.count());

   printf("Seeking to 100,000\n");

   tr.seek(106000);

   printf("Seeking to 0\n");

   tr.seek(0);
   
   string acc("");
   while(tr.pos() < tr.count()) {

      pintrace::Frame *f = tr.next();

     // printf("No: %u Type: %d\n", tr.pos(), f->type);
      
      switch(f->type) {
          case FRM_STD:
          {
            pintrace::StdFrame *sf = (pintrace::StdFrame *) f;
            printf("Standard frame.\n");
            printf("Addr: 0x%x\n", sf->addr);
            printf("TID: %d\n", sf->tid);
            printf("Insn length: %d\n", sf->insn_length);
            printf("Values count: %d\n", sf->values_count);

            printf("Insn bytes: ");

            unsigned int len =
               (sf->insn_length == 0) ? MAX_INSN_BYTES : sf->insn_length;

            for (unsigned int i = 0; i < len; i++) {
               printf("%02hhx ", sf->rawbytes[i]);
            char temp[10];
            sprintf(temp, "\\x%02hhx", sf->rawbytes[i]);
            acc += string(temp);
            }

            printf("\n  Val\t\t  Taint\n");
            printf("\n--------------------------------\n");
            for (uint32_t i = 0 ; i < sf->values_count ; i ++)
               printf("%8x\t%8x\n", sf->values[i], sf->taint[i]);

            printf("=================================\n");

            delete f;
            break;
         }
          case FRM_STD2:
          {
            pintrace::StdFrame2 *sf = (pintrace::StdFrame2 *) f;
            printf("Standard frame (enhanced).\n");
            printf("Addr: 0x%x\n", sf->addr);
            printf("TID: %d\n", sf->tid);
            printf("Insn length: %d\n", sf->insn_length);
            printf("Values count: %d\n", sf->values_count);

            printf("Insn bytes: ");

            unsigned int len =
               (sf->insn_length == 0) ? MAX_INSN_BYTES : sf->insn_length;

            for (unsigned int i = 0; i < len; i++) {
               printf("%02hhx ", sf->rawbytes[i]);
            char temp[10];
            sprintf(temp, "\\x%02hhx", sf->rawbytes[i]);
            acc += string(temp);
            }

            printf("\n  Val\t\t  Taint\n");
            printf("\n--------------------------------\n");
            for (uint32_t i = 0 ; i < sf->values_count ; i ++)
              printf("%8x\t%8x\t%x\n", sf->values[i].dword[0], sf->taint[i], sf->locs[i]);

            printf("=================================\n");

            delete f;
            break;
         }
          case FRM_KEY:
         {
            pintrace::KeyFrame *kf = (pintrace::KeyFrame *) f;
            printf("Keyframe.\n");
            printf("eax = 0x%x\n", kf->eax);
            printf("ebx = 0x%x\n", kf->ebx);
            printf("ecx = 0x%x\n", kf->ecx);
            printf("edx = 0x%x\n", kf->edx);
            printf("frame num = %Lx", kf->pos);
            break;
         }
      case FRM_LOADMOD:
         {
            pintrace::LoadModuleFrame *lm = (pintrace::LoadModuleFrame *) f;
            printf("Load Module.\n");
            printf("name: %s addr: %x to %x\n", lm->name, lm->low_addr, lm->high_addr);
            break;
         }
      case FRM_SYSCALL:
         {
            pintrace::SyscallFrame *sc = (pintrace::SyscallFrame *) f;
            printf("Syscall.\n");
            printf("call no: %d\n", sc->callno);
            break;
         }
          case FRM_TAINT:
          {
            pintrace::TaintFrame *tf = (pintrace::TaintFrame *) f;
            printf ("Taint intro: id %x\n", tf->id);
            break;
          }
        case FRM_EXCEPT:
        {
          pintrace::ExceptionFrame *ef = (pintrace::ExceptionFrame *) f;
          printf ("Exception: id %x thread %x addr %x\n", ef->exception, ef->tid, ef->from_addr);
          break;
        }
          default:
         break;
      }

   }
   //   cerr << acc;
   return 0;
}
