For Windows:

Download `llvm` from `https://sourceforge.net/projects/clangonwin/`

Install into `c:\LLVM`

```cmd
set CLANG_PURE_LLVM_INCLUDE_DIR=c:\llvm\include
set CLANG_PURE_LLVM_LIB_DIR=c:\llvm\lib;c:\llvm\bin;c:\llvm\lib\clang\6.0.0\lib\windows

stack --extra-lib-dirs=c:\llvm\lib --extra-lib-dirs=c:\llvm\bin --extra-lib-dirs=c:\llvm\lib\clang\6.0.0\lib\windows --extra-include-dirs=c:\llvm\include install clang-pure

stack install lens pretty-simple
```

For Linux:

```bash
sudo apt install llvm
stack install clang-pure lens pretty-simple
```
