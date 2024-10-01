cmake_minimum_required(VERSION 3.28)
project(ac)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED True)

macro(add_single_file_targets)
    file(GLOB SRC_FILES "${ARGV0}")
    foreach (SRC_NAME ${SRC_FILES})
        cmake_path(GET SRC_NAME STEM TARGET_NAME)
        add_executable("${ARGV1}_${TARGET_NAME}" ${SRC_NAME})
    endforeach ()
endmacro()

add_single_file_targets(./Basis/*.cpp Basis)
add_single_file_targets(./Polymorphism/*.cpp Poly)
add_single_file_targets(./Templates/*.cpp Templ)