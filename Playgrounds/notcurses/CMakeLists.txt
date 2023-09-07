cmake_minimum_required(VERSION 3.26)
project(notcurses)

set(CMAKE_CXX_STANDARD 23)

include(FeatureSummary)

find_package(Notcurses NAMES Notcurses REQUIRED)

add_executable(p1 main.cpp)
target_link_libraries(p1 PRIVATE ${Notcurses_LIBRARIES})

feature_summary(WHAT ALL DESCRIPTION "Build configuration:")