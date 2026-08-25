#include "tlpi_hdr.h"
#include <stdio.h>
#include <fcntl.h> // file control
#include <sys/stat.h>

int
main ()
{
  // fd is a file descriptor
  int fd;
#define file "test.txt"

  fd = open(file,O_RDWR | O_CREAT | O_TRUNC, (S_IRWXU | S_IRWXG | S_IRWXO) );

  printf ("file describer->%d\n",fd);

  if (fd == -1)
    errExit("open fd");

  if (write(fd, "Hello,", 6) == -1)
    errExit("write1");

  char cmd[] = "cat " file ";echo";
  system(cmd);
}
