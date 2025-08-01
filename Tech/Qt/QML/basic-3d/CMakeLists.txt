cmake_minimum_required(VERSION 3.16)

project(qml3d VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 6.5 REQUIRED COMPONENTS Quick ShaderTools)

qt_standard_project_setup(REQUIRES 6.5)

qt_add_executable(appqml3d
    main.cpp
)

qt_add_qml_module(appqml3d
    URI qml3d
    VERSION 1.0
    QML_FILES Main.qml
)

qt_add_shaders(appqml3d "qml3d"
    PRECOMPILE
    PREFIX
        "/"
    FILES
        "hello.frag"
)

target_link_libraries(appqml3d
    PRIVATE Qt6::Quick
)
