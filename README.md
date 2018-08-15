C Compiler in OCaml

Working from 
https://norasandler.com/2017/11/29/Write-a-Compiler.html

Stage 1
Lexer, recursive descent parser, and codegen.
Can compile ```int main() {return 2;}``` and not much else.

Stage 2
Unary operators ~, ! and - added.

Stage 3
Binary operators +, -, *, and /, with correct precedence and associativity.
Recursive descent is maintained as the parsing method (though Pratt parsing looks like a better bet)

Stage 4
Relational operators: &&, ||, ==, !=, <=, >=, <, >

Stage 5
In progress

```
./compile.sh tests/stage_1/valid/newlines.c
./out.exe
```