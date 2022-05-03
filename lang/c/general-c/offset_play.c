#include <stdio.h>
#include <stddef.h>

struct S {
    char c[1];
    int d;
};

struct S2 {
    char c[7];
    int d;
};

struct S3 {
    char c[9];
    int d;
};

struct S4 {
    char c[12];
    int d;
};

// the memory is aligned to 4bytes chunk, if there are empty, they will be padded.
// 64bits = 8 bytes

int main(){
    printf("%zu\n", offsetof(struct S,d));
    printf("%zu\n", offsetof(struct S2,d));
    printf("%zu\n", offsetof(struct S3,d));
    printf("%zu\n", offsetof(struct S4,d));

    printf("%zu\n", sizeof(char));

}

