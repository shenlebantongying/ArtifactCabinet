#include <cstdio>
#include <cstdlib>
#include <unistd.h>

// [Expected behavior]
// $ ./arg_parsing -n -t asd
// nflag=1; tflag=1; msg=asd; optind=4
// -n activated
// -f activated and msg is asd


#define SUPPORTED_FLAGS "nt:"

int
main (int argc, char *argv[])
{

  int opt;
  bool nflag, tflag;
  char* msg;

  while ((opt = getopt (argc, argv, SUPPORTED_FLAGS)) != -1)
    {
      switch (opt)
        {
        case 'n':
          nflag = true;
          break;
        case 't':
          msg = optarg;
          tflag = true;
          break;
        default:
          fprintf (stderr, "Usage: %s [-t msg] [-n] name\n", argv[0]);
          exit (EXIT_FAILURE);
        }
    }

  printf ("nflag=%d; tflag=%d; msg=%s; optind=%d\n", nflag, tflag, msg,
          optind);

  //  The variable optind is the index of the next element to be
  //  processed in argv.  The system initializes this value to 1.  The
  //  caller can reset it to 1 to restart scanning of the same argv, or
  //  when scanning a new argument vector.


  if (nflag)
    {
      printf ("-n activated \n");
    }

  if (tflag)
    {
      printf ("-f activated and msg is %s\n", msg);
    }

  exit (EXIT_SUCCESS);
}
