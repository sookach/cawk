add_library(AST Decl.cpp)
target_include_directories(AST PRIVATE ${CMAKE_SOURCE_DIR}/cawk/include)