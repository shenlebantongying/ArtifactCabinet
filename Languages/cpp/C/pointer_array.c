#include <stdio.h>
#include <stdlib.h>

#define LENGTH 4

int main(){
    int *numbers;

    numbers = (int *) malloc(sizeof(int)*LENGTH);
    if (numbers == NULL)
    {
        // TODO: when will malloc fail?
        fprintf(stderr,"Cannot alloc memory\n");
        exit(1);
    }

    for (int i = 1; i < LENGTH+1 ; ++i) {
        *(numbers+i) = i*i;
    }

    for (int i = 0; i < LENGTH ; ++i) {
        printf("%d\n",*(numbers+i));
    }
}
