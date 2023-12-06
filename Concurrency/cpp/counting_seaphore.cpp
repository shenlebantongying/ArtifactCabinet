#include <semaphore>
#include <vector>
#include <thread>
#include <iostream>
#include <chrono>
#include <mutex>

// limited tasks but only 2 workers can work at the same time

std::counting_semaphore<2> sea(0);

std::chrono::time_point<std::chrono::steady_clock> start;

std::mutex plock;

void p(std::string_view s) {
    auto now = std::chrono::steady_clock::now();
    std::scoped_lock<std::mutex> g(plock);
    std::cout << s << " at " << std::chrono::duration_cast<std::chrono::seconds>(now - start) << std::endl;
}

void work(std::string i) {
    sea.acquire();
    p(i + " working");
    std::this_thread::sleep_for(std::chrono::seconds(1));
    sea.release();
    p(i + " finished");
}

int main() {

    start = std::chrono::steady_clock::now();

    std::vector<std::thread *> q{};

    for (int i = 0; i < 10; ++i) {
        std::string s = std::to_string(i);
        p(s + " waiting");
        auto *t = new std::thread(work, s);
        q.push_back(t);
    }

    std::cout << "\n";

    sea.release(2);

    for (auto *t: q) {
        t->join();
    }
}