set( LLVM_LINK_COMPONENTS
  Support
  )

add_clang_tool(aterms-dumper 
  ATermsDumper.cpp
  )

clang_target_link_libraries(aterms-dumper
  PRIVATE
  clangLex
  clangAST
  clangBasic
  clangFrontend
  clangSerialization
  clangTooling
  )