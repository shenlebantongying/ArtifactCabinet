#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

#define NTHREADS 100

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;

int counter = 0;

void *ctr_inc(){
    printf("Thread number: %ld\n",pthread_self());

    // All threads will print in the same time
    sleep(2);

    pthread_mutex_lock(&mutex1);

    // if sleep is here, then program will run in sequential order
    // sleep(2);

    counter++;
    printf("%d \n", counter);
    pthread_mutex_unlock(&mutex1);

}


int main() {
    pthread_t thread_id[NTHREADS];

    int rc;
    for (int i = 0; i < NTHREADS; ++i) {
        if((rc=pthread_create(&thread_id[i],NULL, ctr_inc,NULL))){
            printf("thread creation failed %d\n",rc);
        }
    }

    for (int i = 0; i < NTHREADS; ++i) {
        pthread_join(thread_id[i],NULL);
    }

    return 0;
}
