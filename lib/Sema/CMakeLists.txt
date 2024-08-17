add_library(Sema SemaType.cpp SemaLValue.cpp SemaControlFlow.cpp Sema.cpp SymbolResolver.cpp)
target_include_directories(Sema PRIVATE ${CMAKE_SOURCE_DIR}/include)