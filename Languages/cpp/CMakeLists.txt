cmake_minimum_required(VERSION 3.28)
project(ac)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_C_STANDARD 23)

macro(add_single_file_targets)
    file(GLOB SRC_FILES "${ARGV0}")
    foreach (SRC_NAME ${SRC_FILES})
        cmake_path(GET SRC_NAME STEM TARGET_NAME)
        add_executable(${TARGET_NAME} ${SRC_NAME})
    endforeach ()
endmacro()

add_single_file_targets(./Basis/*.cpp)
add_single_file_targets(./Polymorphism/*.cpp)
add_single_file_targets(./Templates/*.cpp)