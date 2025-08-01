#include <iostream>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <cstring>
#include <thread>

// open a existing file as map then write something into it

int main() {
    int fd = open("test.txt", O_RDWR);
    int offset = 5;
    int pa_offset = offset & ~(sysconf(_SC_PAGE_SIZE) - 1);
    /* offset for mmap() must be page aligned */;

    char *addr;
    addr = static_cast<char *>(mmap(nullptr, 10, PROT_WRITE, MAP_SHARED, fd, pa_offset));

    close(fd);

    auto len = std::strlen(addr);
    std::cout << len << std::endl;


    for (int i = 0; i < 999; ++i) {

        if (addr[0] > 'z') {
            addr[0] = 'a';
        } else {
            addr[0] = ++addr[0];
        }

        if (addr[len - 1] == '!') {
            addr[len - 1] = '?';
        } else {
            addr[len - 1] = '!';
        }

        std::cout << addr << std::endl;

        std::this_thread::sleep_for(std::chrono::seconds(1));
    }


    munmap(addr, 10);


    return 0;
}
