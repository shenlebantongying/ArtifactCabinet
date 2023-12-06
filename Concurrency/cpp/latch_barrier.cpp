#include <latch>
#include <iostream>
#include <thread>
#include <mutex>
#include <chrono>
#include <stack>

// finish until a list of items are all consumed.

const int N = 10;

std::latch l(N);

std::stack<int> v;
std::mutex vlock;

std::mutex plock;

void p(std::string s) {
    std::scoped_lock<std::mutex> g(plock);
    std::cout << s << std::endl;
}

int main() {

    for (int i = 0; i < N; ++i) {
        v.push(i);
    }

    for (int i = 0; i < N; ++i) {
        std::thread([] {
            int cur_work;

            vlock.lock();
            cur_work = v.top();
            v.pop();
            vlock.unlock();

            std::this_thread::sleep_for(std::chrono::seconds(1));
            p(std::to_string(cur_work) + " done.");

            l.count_down();
        }).detach();
    }

    l.wait();

}