C Compiler in OCaml

Working from 
https://norasandler.com/2017/11/29/Write-a-Compiler.html

Stage 1
Lexer, recursive descent parser, and codegen.
Can compile ```int main() {return 2;}``` and not much else.

Stage 2
Unary operators ~, ! and - added.

Stage 3
In progress

```
./compile.sh tests/stage_1/valid/newlines.c
./out.exe
```