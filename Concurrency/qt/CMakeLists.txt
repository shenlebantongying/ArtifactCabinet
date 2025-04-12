cmake_minimum_required(VERSION 3.31)
project(map_reduce)

set(CMAKE_CXX_STANDARD 26)

find_package(Qt6 COMPONENTS
        Core
        Concurrent
        REQUIRED)

file(GLOB MFILES "./*.cpp")
foreach (F ${MFILES})
    cmake_path(GET F STEM F_STEM)
    add_executable("target_${F_STEM}" ${F})
    target_link_libraries("target_${F_STEM}" Qt::Core Qt6::Concurrent)
endforeach ()

