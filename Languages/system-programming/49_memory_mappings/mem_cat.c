#include "tlpi_hdr.h"
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>

int
main (int argc, char *argv[])
{
  char *addr;
  int fd;         // file descriptor
  struct stat fs; // file stats

  if (argc != 2 || strcmp (argv[1], "--help") == 0)

    usageErr ("%s file\n", argv[0]);

  fd = open (argv[1], O_RDONLY);
  if (fd == -1)
    errExit ("open");

  if (fstat (fd, &fs) == -1)
    errExit ("fstat");

  addr = mmap (NULL, fs.st_size, PROT_READ, MAP_PRIVATE, fd, 0);

  // PROT_READ means the page may be read

  if (addr == MAP_FAILED)
    errExit ("mmap");

  if (write (STDOUT_FILENO, addr, fs.st_size) != fs.st_size)
    fatal ("Partial write");

  exit (EXIT_SUCCESS);
}
