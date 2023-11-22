cmake_minimum_required(VERSION 3.25)
project(copy_move_sematics)

set(CMAKE_CXX_STANDARD 23)

add_executable(1 move_copy_ctrs.cpp)

add_executable(2 vector_shared_ptr_move.cpp)
