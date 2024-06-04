cmake_minimum_required(VERSION 3.29)
project(pdfOutlineEdit)

set(CMAKE_CXX_STANDARD 26)

include_directories("/opt/homebrew/include")

find_package(PkgConfig)
pkg_check_modules(deps REQUIRED IMPORTED_TARGET libpodofo fmt)


add_library(libPdfOutline SHARED pdf_outline.cpp)
target_link_libraries(libPdfOutline PUBLIC PkgConfig::deps)


add_executable(cli cli.cpp)
target_link_libraries(cli PUBLIC libPdfOutline)