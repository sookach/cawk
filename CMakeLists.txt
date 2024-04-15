cmake_minimum_required(VERSION 3.26)

project(cawk)


set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON) 

include_directories(include)

file(GLOB SOURCES "src/*.cc")

add_executable(cawk ${SOURCES})