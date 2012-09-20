#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>


void impossible()
{
   printf("Its impossible to execute this function\n");
}

void test(char * buf)
{
   char local[8];
   int i;
   strcpy(local, buf);
}

int fd;

int main(int argc, char *argv[])
{
  char buf[500];
  size_t count;
  fd = open("readme", O_RDONLY);
  if(fd == -1) {
    perror("open");
    exit(-1);
  }
  count = read(fd, buf, 500);
  if(count == -1) {
    perror("read");
    exit(-1);
  }

  if(buf[0] != 'h') {
    printf("the file contents must start with h\n");
	close(fd);
    exit(-1);
  }

  close(fd);

  test(buf);
  return 0;
}
