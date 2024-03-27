cmake_minimum_required(VERSION 3.29)
project(arrow_apache)

set(CMAKE_CXX_STANDARD 26)

find_package(PkgConfig)
pkg_check_modules(arrow REQUIRED IMPORTED_TARGET arrow)

add_executable(basis basis.cpp)
target_link_libraries(basis PRIVATE PkgConfig::arrow)
