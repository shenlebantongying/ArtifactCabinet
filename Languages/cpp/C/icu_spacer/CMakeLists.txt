cmake_minimum_required(VERSION 3.25)
project(icu_spacer C)

set(CMAKE_C_STANDARD 11)

find_package(ICU REQUIRED COMPONENTS data i18n io tu uc )

add_executable(icu_spacer cjkpad.c)
target_link_libraries(icu_spacer PRIVATE ${ICU_LIBRARIES})