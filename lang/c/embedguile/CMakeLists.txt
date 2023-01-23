cmake_minimum_required(VERSION 3.23)
project(embed_guile)

set(CMAKE_CXX_STANDARD 20)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GUILE3 REQUIRED guile-3.0)

add_executable(embed_guile main.cpp)

target_include_directories(embed_guile PRIVATE ${GUILE3_INCLUDE_DIRS})
target_link_libraries(embed_guile PRIVATE ${GUILE3_LINK_LIBRARIES})