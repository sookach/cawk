add_library(Sema SemaDecl.cpp SemaType.cpp SemaLValue.cpp)
target_include_directories(Sema PRIVATE ${CMAKE_SOURCE_DIR}/include)