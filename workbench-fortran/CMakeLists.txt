cmake_minimum_required(VERSION  3.14)
project(my_proj Fortran)
enable_language(Fortran)

# other flags
# -fbounds-check : boundary check, but very slow.

set (CMAKE_Fortran_FLAGS   "${CMAKE_Fortran_FLAGS} -std=f2018 -Wall -Wextra -Wconversion")

file(GLOB SRC_FILES *.f90)

foreach(SRC ${SRC_FILES})
    # CMAKE 3.14 feature -> extract barename
    get_filename_component(BARE_NAME ${SRC} NAME_WE)
    add_executable(${BARE_NAME} ${SRC})
endforeach()

configure_file(04_data.txt 04_data.txt COPYONLY)
