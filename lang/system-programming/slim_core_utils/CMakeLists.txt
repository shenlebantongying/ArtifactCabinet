add_executable(arg_parsing arg_parsing.cpp)

include_directories(${CMAKE_SOURCE_DIR}/include)

target_link_libraries(arg_parsing PRIVATE utils)