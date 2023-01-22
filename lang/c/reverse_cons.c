// https://cs.uwaterloo.ca/~plragde/flaneries/IYMLC/Pointers.html#%28part._.List_.Exercises%29
// Exercise 4
#include <stdlib.h>
#include <stdio.h>

typedef struct node {
    int first;
    struct node *rest;
} node_t;

node_t *cons(int n, node_t *node) {
    node_t *ans = (node_t *) malloc(sizeof(node_t));
    ans->first = n;
    ans->rest = node;
    return ans;
}

void printList(node_t *lst) {
    node_t *p = lst;
    while (p != NULL) {
        printf("%d", p->first);
        if (p->rest == NULL) {
            printf("nice");
            break;
        } else {
            p = p->rest;
        }
    }
}

// make a new list in reverse order
node_t * reverse(node_t *node) {
    node_t *ans;
    for (node_t *p = node; p != NULL; p = p->rest) {
        ans = cons(p->first, ans);
    }
    return ans;
}

int main() {
    node_t *t = cons(5, cons(4, cons(3, NULL)));
    node_t *reversed = reverse(t);
    printList(reversed);
}