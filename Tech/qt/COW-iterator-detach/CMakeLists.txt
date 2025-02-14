cmake_minimum_required(VERSION 3.31)
project(COW_iterator_detach)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_AUTOMOC ON)

find_package(Qt6 COMPONENTS
        Core
        REQUIRED)

add_executable(COW_iterator_detach main.cpp)
target_link_libraries(COW_iterator_detach
        Qt::Core
)

