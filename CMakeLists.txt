cmake_minimum_required(VERSION 3.26)

project(cawk)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON) 
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fexperimental-library")

add_executable(cawk cawk.cc)
add_executable(main main.cc)