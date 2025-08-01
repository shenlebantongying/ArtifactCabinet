cmake_minimum_required(VERSION 3.17)
project(advanced_cpp)

set(CMAKE_CXX_STANDARD 20)

add_executable(vpi vector_pointer_invalidation.cpp)
add_executable(modify_stack_of_previous_var modify_stack_of_previous_var.cpp)
