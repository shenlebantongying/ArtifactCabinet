cmake_minimum_required(VERSION 3.19)
project(gdk_keyevent C)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GTK REQUIRED IMPORTED_TARGET gtk4 glib-2.0)

set(CMAKE_C_STANDARD 23)
add_compile_definitions(_GNU_SOURCE)

add_executable(gk main.c)
target_link_libraries(gk PRIVATE PkgConfig::GTK)
