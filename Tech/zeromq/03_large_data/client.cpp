#include <print>
#include <thread>
#include <zmq.h>

void error_check(int rc)
{
    if (rc < 0) {
        std::println("{} -> errno {} -> {}", rc, errno, zmq_strerror(errno));
        exit(1);
    }
}

int main()
{
    void* ctx = zmq_ctx_new();
    void* socket = zmq_socket(ctx, ZMQ_REQ);
    int rc = zmq_connect(socket, "tcp://localhost:5558");
    error_check(rc);

    int loop = 3;
    while (loop-- > 0) {
        std::println("client loop!");
        std::string msg = "msg!";
        for (auto i = 0; i < 1000; ++i) {
            msg.append(std::format("{} ", i));
        }
        const char* l = "msg!";
        error_check(zmq_send(socket, msg.data(), msg.length(), 0));

        char buf[256];
        int len = zmq_recv(socket, buf, sizeof(buf), 0);
        error_check(len);

        buf[len] = '\0';

        printf("received -> %s!\n", buf);
    }
}
