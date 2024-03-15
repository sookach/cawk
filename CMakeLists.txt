cmake_minimum_required(VERSION 3.26)

project(cawk)

find_package(LLVM REQUIRED CONFIG)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON) 
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fexperimental-library")

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(include)

add_definitions(${LLVM_DEFINITIONS})

file(GLOB SOURCES "src/*.cc")

add_executable(cawk ${SOURCES})
add_executable(main main.cc)

llvm_map_components_to_libnames(llvm_libs support)

target_link_libraries(cawk ${llvm_libs})