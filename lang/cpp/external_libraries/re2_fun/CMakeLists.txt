cmake_minimum_required(VERSION 3.10)
project(my_project)
set(CMAKE_CXX_STANDARD 20)

find_package(PkgConfig REQUIRED)
pkg_check_modules(RE2 REQUIRED re2)

add_executable(re2_play re2_play.cpp)

target_link_libraries(re2_play ${RE2_LIBRARIES})
target_include_directories(re2_play PUBLIC ${RE2_INCLUDE_DIRS})
target_compile_options(re2_play PUBLIC ${RE2_CFLAGS_OTHER})