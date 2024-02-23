cmake_minimum_required(VERSION 3.28)
project(google_benchmark)

set(CMAKE_CXX_STANDARD 23)

find_package(PkgConfig)
pkg_check_modules(PKGS IMPORTED_TARGET benchmark fmt)
find_package(Qt6 REQUIRED COMPONENTS Core)

add_executable(g str_formatting.cpp)
target_link_libraries(g PRIVATE PkgConfig::PKGS Qt6::Core)
