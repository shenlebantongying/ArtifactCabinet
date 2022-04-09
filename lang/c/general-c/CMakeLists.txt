cmake_minimum_required(VERSION 3.19)
project(general_c C)

set(CMAKE_C_STANDARD 11)

add_executable(niceStr appendStr.c)
add_executable(pointer_array pointer_array.c)