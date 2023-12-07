cmake_minimum_required(VERSION 3.17)
project(ac)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_C_STANDARD 23)

#set(ECM_ENABLE_SANITIZERS "address;undefined")

#find_package(ECM REQUIRED NO_MODULE)
#set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH})

#include(ECMEnableSanitizers)

add_executable(crtp_basic Polymorphism/CRTP_basic.cpp)
add_executable(func_ptr Polymorphism/func_ptr.cpp)
add_executable(plygin_impl Polymorphism/plugin_policy.cpp)

file(GLOB SRC_FILES ./Basis/*.cpp)
foreach (SRC_NAME ${SRC_FILES})
    cmake_path(GET SRC_NAME STEM TARGET_NAME)
    add_executable(${TARGET_NAME} ${SRC_NAME})
endforeach ()