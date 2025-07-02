#include <fmt/color.h>
#include <fmt/format.h>
#include <iostream>
#include <mutex>
#include <thread>
#include <vector>
#include <zmq.h>

auto endpointName = "inproc://sth";

std::hash<std::thread::id> thread_id_hash;

std::mutex printMutex;
void sPrintln(std::string s)
{
    std::lock_guard<std::mutex> lock(printMutex);
    fmt::print("{}\n", s);
}

void redPrintln(const std::string& s)
{
    std::lock_guard<std::mutex> lock(printMutex);
    fmt::print(fg(fmt::color::red), "{}\n", s);
}

void greenPrintln(const std::string& s)
{
    std::lock_guard<std::mutex> lock(printMutex);
    fmt::print(fg(fmt::color::green), "{}\n", s);
}

void checkError(int rec_code)
{
    if (rec_code != 0) {
        sPrintln(fmt::format("Error: {} -> {} -> {}", rec_code, errno, strerror(errno)));
        std::exit(1);
    }
}

[[noreturn]] void server_thread(void* zmq_ctx)
{
    void* socket = zmq_socket(zmq_ctx, ZMQ_REP);
    int rc = zmq_bind(socket, endpointName);
    checkError(rc);

    size_t t_id = thread_id_hash(std::this_thread::get_id());

    sPrintln(fmt::format("Serv thread id -> {}", t_id));

    int count = 0;

    while (true) {
        std::vector<char> buf {};
        buf.resize(256);

        zmq_recv(socket, buf.data(), buf.size(), 0);
        redPrintln(fmt::format("Server GETT < {}", buf.data()));

        count += 1;

        std::this_thread::sleep_for(std::chrono::seconds(1));
        std::string t = fmt::format("(serv_msg {})", count);
        zmq_send(socket, t.data(), t.size(), 0);
    }
}

void client_thread(void* zmq_ctx)
{
    void* socket = zmq_socket(zmq_ctx, ZMQ_REQ);
    int rc = zmq_connect(socket, endpointName);
    checkError(rc);

    size_t t_id = thread_id_hash(std::this_thread::get_id());

    for (int i = 0; i < 3; ++i) {
        std::string msg = fmt::format("(msg {} from {})", i, t_id);

        zmq_send(socket, msg.data(), msg.size(), 0);

        greenPrintln(fmt::format("Client SENT > {}", msg));

        std::vector<char> rec;
        rec.resize(256);
        zmq_recv(socket, rec.data(), rec.size(), 0);
        greenPrintln(fmt::format("Client GETT < {}", rec.data()));
    }

    zmq_close(socket);
}

int main()
{
    void* zmq_ctx = zmq_ctx_new();

    auto* mthread = new std::thread(server_thread, zmq_ctx);
    mthread->detach();

    auto* cthread1 = new std::thread(client_thread, zmq_ctx);
    auto* cthread2 = new std::thread(client_thread, zmq_ctx);

    cthread1->join();
    cthread2->join();

    sPrintln("Process finished");

    return 0;
}
