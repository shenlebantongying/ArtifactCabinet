cmake_minimum_required(VERSION 3.23)
project(pthreads_c C)

set(CMAKE_C_STANDARD 23)

file(GLOB SOURCES LIST_DIRECTORIES false *.c)

foreach (SRC ${SOURCES})
    cmake_path(GET SRC FILENAME SRC_PATH)
    cmake_path(GET SRC_PATH STEM SRC_STEM)
    add_executable(${SRC_STEM} ${SRC_PATH})
endforeach ()

