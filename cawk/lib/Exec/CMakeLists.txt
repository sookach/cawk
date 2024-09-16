add_library(Exec Exec.cpp IO.cpp)
target_include_directories(Exec PRIVATE ${CMAKE_SOURCE_DIR}/cawk/include)