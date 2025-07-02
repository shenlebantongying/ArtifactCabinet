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
    void* socket = zmq_socket(ctx, ZMQ_REP);
    int rc = zmq_bind(socket, "tcp://*:5558");
    error_check(rc);

    while (true) {
        std::println("\nloop!");
        std::string longMsg;

        while (true) {
            zmq_msg_t msg;
            error_check(zmq_msg_init(&msg));
            int len = zmq_msg_recv(&msg, socket, 0);
            error_check(len);

            longMsg.append(std::string_view((char*)(zmq_msg_data(&msg)), zmq_msg_size(&msg)));
            if (!zmq_msg_more(&msg)) {
                break;
            }
            zmq_msg_close(&msg);
        }

        std::println("received msg ->{}", longMsg);

        const char* ok = "ok!";
        zmq_send(socket, ok, strlen(ok), 0);
    }
}
