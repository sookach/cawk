cmake_minimum_required(VERSION 3.28)

project(cawk)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_subdirectory(lib)

add_executable(main main.cpp)
target_include_directories(main PRIVATE include)
target_link_libraries(main PRIVATE Basic Lexer Parser)
