cmake_minimum_required(VERSION 3.31)
project(array_binary_format_file)

set(CMAKE_CXX_STANDARD 26)

find_package(Qt6 COMPONENTS
        Core
        REQUIRED)

add_executable(array_binary_format_file main.cpp)
target_link_libraries(array_binary_format_file
        Qt::Core
)

