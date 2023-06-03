# Compilation

1. [Getting the Source Code and Building LLVM](https://llvm.org/docs/GettingStarted.html#id4). Especially, `-DLLVM_ENABLE_PROJECTS` should at least have *clang*.
2. After compiling, an executable *aterms-dumper* resides in *llvm-project/build/bin/*.

# Run

Assume that you are in the *build* directory.
For instance, `./bin/aterms-dumper ./casestudy.cpp`
