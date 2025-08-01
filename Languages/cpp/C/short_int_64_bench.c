#include <time.h>
#include <stdio.h>
#include <limits.h>

// Bad benchmark: The time cost difference is mostly on v += i
// (adding shorts cost slightly more);
void loopShorts(){
    unsigned short v = 1;
    for (unsigned short i = SHRT_MAX; i > 0; --i)
        v += i; // add two
}
void loopInts(){
    unsigned int v = 1;
    for (unsigned int i = SHRT_MAX; i > 0; --i)
        v += i;
}

int main(){
    struct timespec before,after;

    #define LOOP_TIMES 4000

    unsigned long long acc = 0;
    for (int i = 0; i < LOOP_TIMES; ++i) {
        clock_gettime(CLOCK_MONOTONIC, &before);
        loopShorts();
        clock_gettime(CLOCK_MONOTONIC,&after);
        acc+=after.tv_nsec-before.tv_nsec;
    }
    printf("short avg ns %lli\n",acc/LOOP_TIMES);

    acc = 0;
    for (int i = 0; i < LOOP_TIMES; ++i) {
        clock_gettime(CLOCK_MONOTONIC, &before);
        loopInts();
        clock_gettime(CLOCK_MONOTONIC,&after);
        acc+=after.tv_nsec-before.tv_nsec;
    }
    printf("int avg ns %lli\n",acc/LOOP_TIMES);

}
