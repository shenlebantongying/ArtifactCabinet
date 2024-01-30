cmake_minimum_required(VERSION 3.27)
project(persistent_stack)

set(CMAKE_CXX_STANDARD 23)


add_executable(stack1
        Stack_impl_ref_count.cpp)

add_executable(stack2
        Stack_impl_arena_alloc.cpp)

#
set(CMAKE_COMPILE_WARNING_AS_ERROR ON)
target_compile_options(stack2 PRIVATE -Wall -Wextra -Wpedantic -Werror)
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=address")
set(CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=address")
