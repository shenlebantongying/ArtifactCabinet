cmake_minimum_required(VERSION  3.14)
project(my_proj Fortran)
enable_language(Fortran)

# other flags
# -fbounds-check : boundary check, but very slow.

set(CMAKE_COMPILE_WARNING_AS_ERROR ON)

set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}")

find_package(BLAS)
find_package(LAPACK)
link_libraries(BLAS::BLAS LAPACK::LAPACK)

file(GLOB SRC_FILES *.f90)

foreach (SRC_NAME ${SRC_FILES})
    cmake_path(GET SRC_NAME STEM TARGET_NAME)
    add_executable(${TARGET_NAME} ${SRC_NAME})
endforeach ()

configure_file(04_data.txt 04_data.txt COPYONLY)
