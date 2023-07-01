#include <iostream>
#include <vector>

using vc = std::vector<char>;

vc genRandom() {
    return {'a', 'b', 'c'};
}

void move(vc &&source, vc &target) {
    target = std::move(source);
}

int main() {
    vc a;
    {
        move(genRandom(), a);
    }

    std::cout << "Hello, World!" << std::endl;
    return 0;
}
