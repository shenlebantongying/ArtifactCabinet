// related to dan grossman's course on closure

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

// The main point is that we have to pass env explictly in VOID!
// Bad practice!

typedef struct List list_t;
struct List {
    void * head;
    list_t  * tail;
};

list_t * makelist (void * x, list_t * xs) {
    list_t * ans = (list_t *)malloc(sizeof (list_t));
    ans->head = x;
    ans->tail = xs;
    return ans;
}

list_t * map(void* (*f)(void*, void*), void* env,list_t * xs){
    if (xs == NULL)
        return NULL;
    else
        return makelist(f(env,xs->head),map(f,env,xs->tail));
                    //    ^ the point of env here?
                    // There is no guarantee about what's inside env!
}

list_t * filter(bool (*f)(void*,void*), void*env, list_t * xs){
    if (xs == NULL)
        return NULL;
    else if (f(env,xs->head))
        return makelist(xs->head,filter(f,env,xs->tail));
    return filter(f,env,xs->tail);
}

int length (list_t* xs){
    int ans = 0;
    while(xs != NULL) {
        ++ans;
        xs = xs -> tail;
    }
    return ans;
}

int main(){
    int temp = 3;
    list_t * a= makelist(&temp,NULL);

    temp=4;
    a= makelist(&temp,a);
    printf("%i\n", *(int*) a->head);

    printf("%i\n", length(a));
}
