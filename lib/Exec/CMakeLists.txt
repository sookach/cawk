add_library(Exec Exec.cpp Format.cpp Sprintf.cpp)
target_include_directories(Exec PRIVATE ${CMAKE_SOURCE_DIR}/include)