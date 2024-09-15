add_library(Lexer Lexer.cpp)
target_include_directories(Lexer PRIVATE ${CMAKE_SOURCE_DIR}/cawk/include)
