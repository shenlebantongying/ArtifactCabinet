cmake_minimum_required(VERSION 3.19)
project(adwplay C)

find_package(PkgConfig REQUIRED)
pkg_check_modules(ALL REQUIRED IMPORTED_TARGET gtk4 glib-2.0 libadwaita-1 gtksourceview-5)

set(CMAKE_C_STANDARD 23)
add_compile_definitions(_GNU_SOURCE)

add_executable(meditor mini-editor.c)
target_link_libraries(meditor PRIVATE PkgConfig::ALL)
