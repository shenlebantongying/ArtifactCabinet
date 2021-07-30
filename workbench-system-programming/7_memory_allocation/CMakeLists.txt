add_executable(memory_basis memory_basis.c)

include_directories(${CMAKE_SOURCE_DIR}/include)

target_link_libraries(memory_basis PRIVATE utils)