add_library(Exec Exec.cpp Format.cpp Index.cpp IO.cpp Split.cpp Sprintf.cpp Value.cpp)
target_include_directories(Exec PRIVATE ${CMAKE_SOURCE_DIR}/include)