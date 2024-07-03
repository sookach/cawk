add_library(
    Basic
    OperatorPrecedence.cpp
    TokenKinds.cpp
)
target_include_directories(Basic PRIVATE ${CMAKE_SOURCE_DIR}/include)
