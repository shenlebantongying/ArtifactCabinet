#include <iostream>
#include <future>
#include <thread>
#include <vector>

int task(int x) {
    return x + 4;
}

int main() {

    std::vector<std::future<int>> task_queue{};

    for (int i = 0; i < 10; ++i) {
        std::packaged_task<int(int)> t(task);
        task_queue.emplace_back(t.get_future());
        std::thread(std::move(t), 1).detach();
    }

    for (std::future<int> &a: task_queue) {
        a.wait();
    }

    for (std::future<int> &i: task_queue) {
        std::cout << i.get() << std::endl;
    }


    return 0;
}
