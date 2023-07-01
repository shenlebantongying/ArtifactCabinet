cmake_minimum_required(VERSION 3.25)
project(zstd_play)

set(CMAKE_CXX_STANDARD 23)

find_package(zstd REQUIRED)

add_executable(simple simple_api.cpp)
target_link_libraries(simple PRIVATE zstd::libzstd_shared)

