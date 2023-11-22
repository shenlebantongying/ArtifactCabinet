#include <fmt/format.h>
#include <iostream>
#include <thread>
#include <vector>
#include <zmq.h>

int main()
{
    void* ctx = zmq_ctx_new();
    void* responder = zmq_socket(ctx, ZMQ_REP);
    int rc = zmq_bind(responder, "tcp://*:5555");

    fmt::print("{}\n", rc);

    while (true) {
        std::vector<char> buf {};
        buf.resize(256);

        zmq_recv(responder, buf.data(), buf.size(), 0);
        fmt::print("Got a message -< {}\n", buf.data());

        std::this_thread::sleep_for(std::chrono::seconds(1));
        std::string t = fmt::format("Processed msg <- {}", buf.data());
        zmq_send(responder, t.data(), t.size(), 0);
    }
}
