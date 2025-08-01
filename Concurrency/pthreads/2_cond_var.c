// pthread's cond variable.

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

int c;

pthread_mutex_t c_mutex;
pthread_cond_t c_cond_v;

#define COUNT 5

void* inc(void* t)
{
    long id = (long)t;

    for (int i = 0; i < COUNT; ++i) {
        pthread_mutex_lock(&c_mutex);
        c += 1;

        pthread_cond_signal(&c_cond_v);

        printf("Signal Send. c=%d from %ld \n", c, id);
        pthread_mutex_unlock(&c_mutex);
        sleep(1);
    }
    pthread_exit(NULL);
}

void* watch_count(void*)
{

    pthread_mutex_lock(&c_mutex);
    while (c < (COUNT * 2)) { // This condition is important

        /* NOTICE
         pthread_cond_wait will automatically & atomically unlock mutex.
         */
        pthread_cond_wait(&c_cond_v, &c_mutex);
        printf("Watcher Received. c=%d\n", c);
    }

    pthread_mutex_unlock(&c_mutex);
    pthread_exit(NULL);
}

int main()
{
    c = 0;

    pthread_t inc_threads[3];

    pthread_mutex_init(&c_mutex, NULL);
    pthread_cond_init(&c_cond_v, NULL);

    pthread_create(&inc_threads[0], NULL, watch_count,NULL);
    pthread_create(&inc_threads[1], NULL, inc, (void*)1);
    pthread_create(&inc_threads[2], NULL, inc, (void*)2);

    for (int i = 0; i < 3; ++i) {
        pthread_join(inc_threads[i], NULL);
    }

    pthread_mutex_destroy(&c_mutex);
    pthread_cond_destroy(&c_cond_v);
    pthread_exit(NULL);
}
