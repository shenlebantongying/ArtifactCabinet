cmake_minimum_required(VERSION 3.17)
project(Linux_Kernal_Interface C CXX)

set(CMAKE_C_STANDARD 11)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
find_package(LibUring REQUIRED)

add_executable(ring_cat ring_cat.c)

target_link_libraries(ring_cat LIBURING)
