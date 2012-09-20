#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#define SZ  0x10000
#define BUF_SIZE 2*SZ
int data[1000000]; //make a big data section

void start_rop(char *);

int main (int argc, char *argv[]) {
  FILE * pFile;
  long lSize;
  char * buffer;
  size_t result;

  if(argc<2){
    printf("Usage: %s <rop shellcode.bin>\n", argv[0]);
    return 1;
  }

  pFile = fopen (argv[1], "rb" );
  if (pFile==NULL) {fputs ("File error",stderr); exit (1);}

  fseek (pFile , 0 , SEEK_END);
  lSize = ftell (pFile);
  rewind (pFile);

  assert(lSize < BUF_SIZE/2);
  buffer = (char*) malloc (BUF_SIZE);
  if (buffer == NULL) {fputs ("Memory error",stderr); exit (2);}

  printf("buf=0x%08x\n", (unsigned int)buffer);
  buffer = (char*)((((unsigned int)buffer+SZ-1)/SZ)*SZ);
  printf("roundup buf=0x%08x\n", (unsigned int)buffer);

  result = fread (buffer,1,lSize,pFile);
  if (result != lSize) {fputs ("Reading error",stderr); exit (3);}

  //pass the shellcode
  start_rop(buffer);

  // terminate
  fclose (pFile);
  free (buffer);
  return 0;
}
