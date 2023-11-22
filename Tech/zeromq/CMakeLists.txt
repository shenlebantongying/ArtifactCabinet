cmake_minimum_required(VERSION 3.26)
project(zeromq_play)

set(CMAKE_CXX_STANDARD 23)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

find_package(PkgConfig REQUIRED)
pkg_check_modules(a IMPORTED_TARGET libzmq fmt)


add_executable(echoServer 01_echo_TCP/server.cpp)
add_executable(echoClient 01_echo_TCP/client.cpp)
target_link_libraries(echoServer PkgConfig::a)
target_link_libraries(echoClient PkgConfig::a)

add_executable(tri-threads-inproc 02_echo_inproc/tri-threads-inproc.cpp)
target_link_libraries(tri-threads-inproc PkgConfig::a)