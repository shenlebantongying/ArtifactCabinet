cmake_minimum_required(VERSION 3.21)
project(kabinet)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

find_package(KF5KIO)

find_package(Qt5 COMPONENTS
        Core
        Gui
        Widgets
        REQUIRED)

add_executable(kabinet main.cpp
                       mynice.cpp
                       mynice.h)
target_link_libraries(kabinet
        Qt5::Core
        Qt5::Gui
        Qt5::Widgets
        KF5::KIOCore KF5::KIOFileWidgets KF5::KIOWidgets KF5::KIONTLM
        )
