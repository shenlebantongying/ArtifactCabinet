cmake_minimum_required(VERSION 3.17)
project(Linux_Kernal_Interface C CXX)

set(CMAKE_C_STANDARD 11)

add_subdirectory(include)

add_subdirectory(4_file_io)

add_subdirectory(6_processes)
add_subdirectory(7_memory_allocation)
