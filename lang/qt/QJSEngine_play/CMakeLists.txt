cmake_minimum_required(VERSION 3.25)
project(QJSEngine_play)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)


find_package(Qt5 COMPONENTS
        Core
        Gui
        Widgets
        Qml
        REQUIRED)

add_executable(QJSEngine_play main.cpp)
target_link_libraries(QJSEngine_play
        Qt5::Core
        Qt5::Gui
        Qt5::Widgets
        Qt5::Qml
        )

