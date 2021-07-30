cmake_minimum_required(VERSION 3.19)
project(gdk_keyevent C)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GTK REQUIRED gtk4)
pkg_check_modules(GLIB REQUIRED glib-2.0)

set(CMAKE_C_STANDARD 11)

add_executable(gk main.c)

target_link_libraries(gk PRIVATE ${GTK_LIBRARIES} ${GLIB_LIBRARIES})
target_include_directories(gk PRIVATE ${GTK_INCLUDE_DIRS} ${GLIB_INCLUDE_DIRS})
target_link_directories(gk PRIVATE ${GTK_LIBRARY_DIRS} ${GLIB_LIBRARY_DIRS})

target_compile_definitions(gk PRIVATE ${GTK_CFLAGS_OTHERS})
