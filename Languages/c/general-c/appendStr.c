#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>


void append_str(char ** a,char ** b){
//                   ^ pointer to char *
  asprintf (a,"%s=%s",*a,*b);
}

void call_strings(char const* in){
  char* cmd;
  asprintf(&cmd,"strings %s",in);

  if (system(cmd))
    fprintf(stderr,"Fatal: %s", cmd);
  free(cmd);
}

int main(int argc, char **argv) {
  call_strings(argv[0]);

  char* a= "->";
  char* b= "nice";
  append_str (&a,&b);

  printf ("%s",a);
}
