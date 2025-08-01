add_executable(mem_cat mem_cat.c)

include_directories(${CMAKE_SOURCE_DIR}/include)

target_link_libraries(mem_cat PRIVATE utils)
