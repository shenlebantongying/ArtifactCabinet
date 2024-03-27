cmake_minimum_required(VERSION 3.25)
project(unsafeCppThreads)

set(CMAKE_CXX_STANDARD 26)

add_executable(parallelWrite parallelWrite.cpp)
