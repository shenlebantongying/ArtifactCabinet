cmake_minimum_required(VERSION 3.27)
project(CasEditor)

set(CMAKE_C_STANDARD 23)
set(CMAKE_CXX_STANDARD 23)

find_package(PkgConfig REQUIRED)
pkg_check_modules(cairo IMPORTED_TARGET cairo)

add_library(minidraw SHARED minidraw.cpp)
target_link_libraries(minidraw PkgConfig::cairo)

set_target_properties(minidraw PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_LIST_DIR})
