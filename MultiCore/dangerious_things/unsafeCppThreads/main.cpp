#include <iostream>
#include <thread>
#include <vector>

/// This COF

int main() {
    std::vector<int> a{0};
    auto f = [&]() { a.push_back(a.back() + 1); };
    std::thread t1(f), t2(f), t3(f), t4(f);

    t1.join();
    t2.join();
    t3.join();
    t4.join();

    for (auto x: a) {
        std::cout << x << std::endl;
    }
}
