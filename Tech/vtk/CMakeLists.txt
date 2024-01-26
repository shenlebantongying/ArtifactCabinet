cmake_minimum_required(VERSION 3.28)
project(vtk)

set(CMAKE_CXX_STANDARD 23)

find_package(VTK REQUIRED)

find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets OpenGLWidgets)

set(CMAKE_AUTOMOC ON)

add_executable(mvtk basic_qt_vtk.cpp)

target_link_libraries(mvtk
        PRIVATE
        ${VTK_LIBRARIES}
        Qt6::Core
        Qt6::Gui
        Qt6::Widgets
        Qt6::OpenGLWidgets

)

vtk_module_autoinit(
        TARGETS mvtk
        MODULES ${VTK_LIBRARIES}
)