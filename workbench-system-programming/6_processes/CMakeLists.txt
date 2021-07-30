add_executable(process_basis process_basis.c)

include_directories(${CMAKE_SOURCE_DIR}/include)

target_link_libraries(process_basis PRIVATE utils)