cmake_minimum_required(VERSION 4.0)
project(CG_play)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)

set(CGAL_DO_NOT_WARN_ABOUT_CMAKE_BUILD_TYPE TRUE)
find_package(raylib REQUIRED)
find_package(CGAL REQUIRED)

link_libraries(CGAL::CGAL raylib)
add_compile_options(-Wall -Werror)

add_executable(01_SlowConvexHull 01_SlowConvexHull.cpp)
