#include "include/tlpi_hdr.h"
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>

int
main ()
{
  printf ("%ld",(long) getpid());
}
