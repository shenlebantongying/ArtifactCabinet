cmake_minimum_required(VERSION 3.17)
project(Linux_Kernal_Interface C CXX)

set(CMAKE_C_STANDARD 11)

find_package(PkgConfig REQUIRED)

pkg_check_modules(LIBURING IMPORTED_TARGET liburing)

add_executable(ring_cat ring_cat.c)

target_link_libraries(ring_cat PkgConfig::LIBURING)
