cmake_minimum_required(VERSION 3.25)
project(qt6_newAsync)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)


find_package(Qt6 COMPONENTS
        Core
        Concurrent
        REQUIRED)

add_executable(qt6_newAsync main.cpp)
target_link_libraries(qt6_newAsync
        Qt6::Core
        Qt6::Concurrent
        )

