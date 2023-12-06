cmake_minimum_required(VERSION 3.26)
project(cpp)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)

add_executable(future_vector future_vector.cpp)
add_executable(defered_async defered_async.cpp)
add_executable(counting_seaphore counting_seaphore.cpp)
add_executable(latch latch_barrier.cpp)