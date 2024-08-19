cmake_minimum_required(VERSION 3.29)
project(unix_playgrounds C CXX)

set(CMAKE_C_STANDARD 23)
set(CMAKE_CXX_STANDARD 26)

add_compile_definitions(_GNU_SOURCE)

add_subdirectory(map_file)
add_subdirectory(posix_threads)
add_subdirectory(slim_core_utils)