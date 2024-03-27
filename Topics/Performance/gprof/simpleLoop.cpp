#include <iostream>
#include <printf.h>

int holy() {
    std::printf("Ok");
    return 1;
}

void molly() {
    for (int i = 0; i < 10; ++i) {
        holy();
    }
}


int main() {
    for (int i = 0; i < 10; ++i) {
        molly();
    }
    return 0;
}
