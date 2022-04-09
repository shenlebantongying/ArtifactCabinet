cmake_minimum_required(VERSION 3.17)
project(Linux_Kernal_Interface C CXX)

set(CMAKE_C_STANDARD 11)

# https://cmake.org/cmake/help/latest/module/FindThreads.html
set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

add_executable(pthreads_101 pthreads_101.c)
target_link_libraries(pthreads_101 Threads::Threads)
