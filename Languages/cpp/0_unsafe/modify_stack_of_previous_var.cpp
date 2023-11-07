#include <stdint.h>
#include <stdio.h>

int main() {
    int64_t a = 123456;
    int64_t b = 789123;

    int64_t *addr_b_no_modify = &b;
    int64_t *addr_b_64 = &b;
    int32_t *addr_b_32 = (int32_t *) (&b);

    addr_b_64 -= 1;
    addr_b_32 -= 2;

    // Both Ptr now point to a !!

    printf("%p\n%p\n%p\n%p\n",
           &a,
           addr_b_no_modify,
           addr_b_64,
           addr_b_32);

    printf("%ld\n%ld",
           *addr_b_64,
           *(int64_t *) addr_b_32);
}
