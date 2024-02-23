cmake_minimum_required(VERSION 3.28)
project(sql_viewer)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_AUTOMOC ON)
find_package(Qt6 COMPONENTS Core Gui Widgets Sql REQUIRED)

add_executable(basic_qsqlmodel basic_qsqlmodel.cpp)
target_link_libraries(basic_qsqlmodel Qt::Core Qt::Gui Qt::Widgets Qt::Sql)
