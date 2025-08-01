cmake_minimum_required(VERSION 3.21)
project(ktextediter)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)


find_package(Qt5 COMPONENTS
        Core
        Widgets
        REQUIRED)

find_package(KF5TextEditor)

add_executable(ktextediter main.cpp cent.cpp cent.h)
target_link_libraries(ktextediter
        Qt5::Core
        Qt5::Widgets
        KF5::TextEditor
        )
