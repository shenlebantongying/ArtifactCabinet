cmake_minimum_required(VERSION 3.23)
project(libuv_playground C)

set(CMAKE_C_STANDARD 17)

find_package(PkgConfig REQUIRED)
pkg_check_modules(LIBUV REQUIRED libuv)

add_executable(libuv_playground timer.c)

target_include_directories(libuv_playground PRIVATE ${LIBUV_INCLUDE_DIRS})
target_link_libraries(libuv_playground PRIVATE ${LIBUV_LINK_LIBRARIES})
