open Base
open Lexer

let () =
  let y = 3 + 4 in
  let xs = lex "   a" in
  Stdio.print_endline (if Result.is_ok xs then "DONE" else "ERROR")