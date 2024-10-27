// https://github.com/torvalds/linux/blob/v6.11/include/linux/container_of.h
// http://www.kroah.com/log/linux/container_of.html

// Related -> torvalds' good taste linked list

#include <stdlib.h>
#include <printf.h>
#include <assert.h>

#define same_type_p(a,b) __builtin_types_compatible_p(typeof(a),typeof(b))

#define container_of(ptr, type, member) ({ \
    static_assert(same_type_p(*(ptr),((type *)0)->member),"ptr and type->member type mismatch");\
    ((type *) ((void *)(ptr) - offsetof(type, member))); \
    })

struct mData
{
    char* name;
};

struct mContainer
{
    char* id;
    struct mData data;
};


int main()
{
    struct mData* theData;

    {
        char* s1 = "asd";
        char* s2 = "qwe";
        struct mContainer* theContainer = malloc(sizeof(struct mContainer));
        theContainer->data.name = s1;
        theContainer->id = s2;
        theData = &theContainer->data;
    }

    // We cannot get rid of the name `theContainer` and only uses theData and get its container.
    printf("%s\n", container_of(theData, struct mContainer, data)->id);

    free(container_of(theData, struct mContainer, data));
}
