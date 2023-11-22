cmake_minimum_required(VERSION 3.25)
project(map_file)

set(CMAKE_CXX_STANDARD 23)

configure_file(test.txt ${CMAKE_BINARY_DIR}/test.txt COPYONLY)

add_executable(inc incrementer.cpp)
add_executable(pri printer.cpp)
