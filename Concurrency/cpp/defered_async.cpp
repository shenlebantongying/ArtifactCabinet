#include <iostream>
#include <future>
#include <chrono>

int mul(int *x, int *y) {
    std::chrono::duration start = std::chrono::steady_clock::now().time_since_epoch();
    std::cout << "mul called <- " << std::chrono::duration_cast<std::chrono::milliseconds>(start) << std::endl;
    return *x * *y;
}

int main() {

    int a = 1;
    int b = 2;

    auto defer = std::async(std::launch::deferred, &mul, &a, &b);
    auto immed = std::async(std::launch::async, &mul, &a, &b);

    std::this_thread::sleep_for(std::chrono::seconds(2));

    a = 3;
    b = 4;

    defer.wait();
    immed.wait();
    std::cout << defer.get() << "\n" << immed.get() << std::endl;

}