cmake_minimum_required(VERSION 3.17)
project(ac)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_C_STANDARD 23)

file(GLOB SRC_FILES ./Basis/*.cpp)
foreach (SRC_NAME ${SRC_FILES})
    cmake_path(GET SRC_NAME STEM TARGET_NAME)
    add_executable(${TARGET_NAME} ${SRC_NAME})
endforeach ()

file(GLOB SRC_FILES ./Polymorphism/*.cpp)
foreach (SRC_NAME ${SRC_FILES})
    cmake_path(GET SRC_NAME STEM TARGET_NAME)
    add_executable(${TARGET_NAME} ${SRC_NAME})
endforeach ()
