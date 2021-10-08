cmake_minimum_required(VERSION 3.20)
project(cmake_objc C)

enable_language(OBJC)

add_executable(cmake_objc main.m main.h)

target_link_libraries(cmake_objc "-framework Foundation")