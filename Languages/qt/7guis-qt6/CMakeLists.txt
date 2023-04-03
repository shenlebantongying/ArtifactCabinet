cmake_minimum_required(VERSION 3.5)

project(7guis_qt6 LANGUAGES CXX)

set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(QT NAMES Qt6 COMPONENTS Widgets REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Widgets REQUIRED)

set(TASK_ONE
        task1.cpp
        task1.h
)

set(TASK_TWO
    task2.cpp
    task2.h
    )

if(${QT_VERSION_MAJOR} GREATER_EQUAL 6)
    qt_add_executable(task1
        ${TASK_ONE}
    )
    qt_add_executable(task2
    ${TASK_TWO}
)
else()
  message( FATAL_ERROR "Qt6 only :)" )
endif()

# TODO: what is PRIVATE?
target_link_libraries(task1 PRIVATE Qt${QT_VERSION_MAJOR}::Widgets)
target_link_libraries(task2 PRIVATE Qt${QT_VERSION_MAJOR}::Widgets)
