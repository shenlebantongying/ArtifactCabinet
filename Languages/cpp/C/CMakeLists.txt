cmake_minimum_required(VERSION 3.17)
project(advanced_cpp)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_C_STANDARD 23)

#set(ECM_ENABLE_SANITIZERS "address;undefined")

#find_package(ECM REQUIRED NO_MODULE)
#set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH})

#include(ECMEnableSanitizers)

add_executable(bit_masking bit_masking.c)
add_executable(closure closure.c)
add_executable(deserializing_binary deserializing_binary.c)
add_executable(niceStr appendStr.c)
add_executable(pointer_array pointer_array.c)
add_executable(reverse_cons reverse_cons.c)
add_executable(short_int_64_bench short_int_64_bench.c)
add_executable(macro_container_of macro_container_of.c)
