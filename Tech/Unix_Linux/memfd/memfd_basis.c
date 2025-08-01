/*

* create a memfd
* write some content
* modify it with mmap

*/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#define BUF_SIZE 100

int
main ()
{
  int fd = memfd_create ("hello.txt", 0);
  if (fd == -1)
    {
      exit (errno);
    }

  FILE *file = fdopen (fd, "r+");
  fprintf (file, "Hello!");
  rewind (file);

  char char_buf[BUF_SIZE];
  fread (char_buf, 1, BUF_SIZE, file);
  printf ("read -> %s\n", char_buf);

  off_t pa_offset = 0 & ~(sysconf (_SC_PAGE_SIZE) - 1);
  char *mapped = mmap (nullptr, BUF_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
                       fd, pa_offset);

  printf ("written %lu -> %s\n", strlen (mapped), mapped);

  mapped[0] = '!';

  rewind (file);
  fread (char_buf, 1, BUF_SIZE, file);
  printf ("modified with mmap read -> %s\n", char_buf);

  fclose (
      file); // Have to be here? The memfd will be invalidated if close earlier

  return 0;
}
