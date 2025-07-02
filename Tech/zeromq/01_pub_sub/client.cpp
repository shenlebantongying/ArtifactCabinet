#include <fmt/format.h>
#include <iostream>
#include <string>
#include <unistd.h>
#include <vector>
#include <zmq.h>

int main()
{
    void* ctx = zmq_ctx_new();
    void* socket = zmq_socket(ctx, ZMQ_REQ);
    int rc = zmq_connect(socket, "tcp://localhost:5555");

    fmt::print("{}\n", rc);

    pid_t pid = getpid();

    for (int i = 0; i < 10; ++i) {
        std::string msg = fmt::format("(msg {} from {})", i, pid);

        zmq_send(socket, msg.data(), msg.size(), 0);

        fmt::print("Send -> {}\n", msg);

        std::vector<char> rec;
        rec.resize(256);
        zmq_recv(socket, rec.data(), rec.size(), 0);
        std::cout << rec.data() << std::endl;
        rec.clear();
    }
}
