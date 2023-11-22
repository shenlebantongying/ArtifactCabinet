cmake_minimum_required(VERSION 3.23)
project(advanced_cpp)

set(CMAKE_CXX_STANDARD 20)

file(GLOB SRC_FILES *.cpp)
foreach (SRC_NAME ${SRC_FILES})
    cmake_path(GET SRC_NAME STEM TARGET_NAME)
    add_executable(${TARGET_NAME} ${SRC_NAME})
endforeach ()