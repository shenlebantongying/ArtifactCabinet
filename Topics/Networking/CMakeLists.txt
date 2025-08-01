cmake_minimum_required(VERSION 3.19)
project(cpp_networking)

set(CMAKE_CXX_STANDARD 20)

add_subdirectory(curl_temple)
add_subdirectory(socket/echo_server)


add_executable(playground playground.cpp)
