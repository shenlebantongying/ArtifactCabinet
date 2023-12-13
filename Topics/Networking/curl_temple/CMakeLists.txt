project(curl_temple)

# Doc for FinCURL
# https://cmake.org/cmake/help/latest/module/FindCURL.html
find_package(CURL REQUIRED)
include_directories(${CURL_INCLUDE_DIR})

add_executable(simple simple.cpp simple.h)

target_link_libraries( simple CURL::libcurl )