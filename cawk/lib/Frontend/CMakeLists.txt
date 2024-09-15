add_library(Frontend Instance.cpp cawk_main.cpp)
target_include_directories(Frontend PRIVATE ${CMAKE_SOURCE_DIR}/cawk/include)