cmake_minimum_required(VERSION 3.29)
project(Performance)

set(CMAKE_CXX_STANDARD 26)

#add_compile_options(-pg)
#add_link_options(-pg)

add_executable(sloop simpleLoop.cpp)
