#include <stdio.h>
#include <stddef.h>

struct S {
    char c[8];
    int d;
};

struct S2 {
    char c[10];
    int d;
};

struct S3 {
    char c[17];
    int d;
};


int main(){
    printf("%zu\n", offsetof(struct S,c));
    printf("%zu\n", offsetof(struct S,d));

    printf("%zu\n", offsetof(struct S2,c));
    printf("%zu\n", offsetof(struct S2,d));

    printf("%zu\n", offsetof(struct S3,c));
    printf("%zu\n", offsetof(struct S3,d));

    printf("%zu\n", sizeof(char));

}

