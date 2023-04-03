#include <stdlib.h>
#include <stdio.h>
#include "ll.h"



int list_add(list_entry **list, void *val) {

    list_entry *new = (list_entry *) malloc (sizeof (list_entry));

    if(new==NULL){
        return 0;
    }

    new->val=val;

    if((*list) != NULL){
        new->perv = NULL;
        new->next = (*list);
        (*list)->perv=new;
        (*list)=new;
    } else {
        new->perv = NULL;
        new->next = NULL;
        (*list) =new;
    }
    return 1;
}

void *list_find(const list_entry *list, const void *data, list_cmp_func lcf) {
    for (const list_entry * i=list; i != NULL ; i=i->next) {
        if(lcf(i->val,data)==0){
            return i->val;
        }
    }
    return NULL;
}

void list_apply_int(list_entry *list, void (*func)(int *)) {
    list_entry *l = list;
    while(l!=NULL){
        func(l->val);
        l=l->next;
    }
}

// [Custom functions]

// cmp functions
int eq(const void * a, const void *b){
    if(a==b){
        return 0;
    } else {
        return 1;
    }
};

// more generic solution
int cmp(const void *a,  const void *b){
    //reference : std qsort() cmp definition

    return ( *(const int*)a - *(const int*)b ); //What is a and b?

}

// apply functions
void inc1 (int *x) {
    *x=*x+1;
}

void triple (int *x) {
    *x=*x**x**x;
}

int main(){
    int v = 1;
    list_entry *test=NULL;

    list_add(&test, &v);
    printf(" v-> %d\n",*((int *)test->val));

    // Note, as we assign the pointer inside,
    // the linked list will change when v changed.
    v=2;
    printf("%d\n",*((int *)test->val));

    int v2=3;
    list_add(&test, &v2);
    printf("%d\n",*((int *)test->val));
    printf("%d\n",*((int *)test->next->val));

    // This will result error if v2 is not fixed!
    printf("Find v2? -> %d\n",*((int*) list_find(test,&v2,eq)));

    int v3 = 4;
    if(list_find(test,&v3,eq)==NULL){
        printf("cannot find v3\n");
    }

    printf("original-> %d\n",*((int *)test->val));
    list_apply_int(test, inc1);
    printf("inc1 -> %d\n",*((int *)test->val));
    list_apply_int(test, triple);
    printf("triple->%d\n",*((int *)test->val));

    printf("v2->%d v3->%d cmp->%d\n",v2,v3,cmp(&v2,&v3));

    return 0;
}
