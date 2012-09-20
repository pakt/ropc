#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#define NUMINTS  (15)
#define FILESIZE (NUMINTS * sizeof(char))
#define LOCATION (0x3FFFF000)

int main(int argc, char *argv[])
{
    int i;
    int fd;
    char *map;  /* mmapped array of int's */

    char filepath[1024];

    void (*jump) (void) = (void (*)())LOCATION;
    
    snprintf(filepath, sizeof(filepath), "%s%s", get_current_dir_name(), "/mmapped.bin");

    printf("file is %s\n", filepath);
    
    fd = open(filepath, O_RDONLY);
    
    if (fd == -1) {
        perror("Error opening file for reading");
        exit(EXIT_FAILURE);
    }

    map = (char *)mmap((void *)LOCATION, FILESIZE, PROT_READ, MAP_SHARED, fd, 0);
    if (map == MAP_FAILED) {
        close(fd);
        perror("Error mmapping the file");
        exit(EXIT_FAILURE);
    }
    
    /* Read the file int-by-int from the mmap
     */
    for (i = 0; i <=NUMINTS; ++i) {
        printf("%d: 0x%x @ addr %p\n", i, map[i], &(map[i]));
    }

    /* Make the memory executable */
    if(mprotect((void *)LOCATION, FILESIZE, PROT_EXEC)) {
        perror("Could not mprotect memorey to be executable");
        exit(EXIT_FAILURE);
    }
    
    jump();
    
    if (munmap(map, FILESIZE) == -1) {
        perror("Error un-mmapping the file");
    }

    printf("Finished succesfully!\n");
    
    close(fd);
    return 0;
}
