open Base
open Lexer

let () =
  let xs = lex "  { int" in
  Stdio.print_endline (if Result.is_ok xs then "DONE" else "ERROR")