/*
 * Double linked list
 * TODO: generic macro for void <=> int;
 */

#ifndef LL_H
#define LL_H

typedef struct node {
    void *val;          // => arbitrary data type
    struct node *perv;
    struct node *next;
} list_entry;

typedef int (*list_cmp_func)(const void *, const void *);

int list_add(list_entry **list,void *val);

void * list_find(const list_entry *list,
                 const void * data,
                 list_cmp_func lcf);

void list_apply_int(list_entry *list, void (*func)(int *));

// TODO
void * list_pop(list_entry **list, const void * data, list_cmp_func lcf);
void list_sort(list_entry *list, list_cmp_func lcf);
void list_reverse(list_entry **list);
void greater(void *, void*);

// FIXME? is it possible to pass generic int func?
#endif //LL_H
