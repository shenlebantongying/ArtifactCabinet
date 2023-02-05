cmake_minimum_required(VERSION 3.25)
project(circle_qpainter)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)


find_package(Qt6 COMPONENTS
        Core
        Gui
        Widgets
        REQUIRED)

add_executable(circle_qpainter main.cpp protractor.h protractor.cpp)
target_link_libraries(circle_qpainter
        Qt::Core
        Qt::Gui
        Qt::Widgets
        )

