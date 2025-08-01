cmake_minimum_required(VERSION 3.26)
project(graph)

set(CMAKE_CXX_STANDARD 23)

find_package(fmt)

add_executable(graph simple_grid_path_finding.cpp)
target_link_libraries(graph fmt::fmt)
