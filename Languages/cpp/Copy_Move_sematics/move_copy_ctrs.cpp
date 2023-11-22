#include <iostream>

// Testing constructors & passing

class s {
public:
    int i;

    explicit s(int i) : i(i) { printf("s default constructor\n"); }

    ~s() { printf("s deconstructor\n"); }

    s(const s &s) : i(s.i) { printf("s copy constructor\n"); }

    s(s &&) noexcept { printf("s move constructor\n"); }
};


class s_no_copy_move {
public:
    int i{};

    explicit s_no_copy_move(int i) : i(i) { printf("scm default constructor\n"); }

    ~s_no_copy_move() { printf("scm deconstructor\n"); }
};

void Use(s a) {
    std::cout << "use s" << std::endl;
}


void Use(s_no_copy_move a) {
    std::cout << "use s_no_copy_move" << std::endl;
}

void UseX(s_no_copy_move &&a) {
    std::cout << "use s" << std::endl;
}


int main() {

    auto *a = new s(1);
    auto *b = new s_no_copy_move(1);
    std::cout << "1" << std::endl;
    Use(*a);

    std::cout << "\n2" << std::endl;
    Use(std::move(*a));

    std::cout << "\n3" << std::endl;
    Use(*b);

    std::cout << "\n4" << std::endl;
    UseX(std::move(*b));

    std::cout << a->i << b->i << std::endl;
    std::cout << "ok" << std::endl;
    return 0;
}
