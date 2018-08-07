open Base

type token =
| OPEN_CURLY
| CLOSE_CURLY
| OPEN_ROUND
| CLOSE_ROUND
| SEMICOLON
| KEYWORD_INT
| KEYWORD_RETURN
| IDENTIFIER
| INT_LITERAL

let token_regexs = Map.of_alist_exn (module String) [
  "{", OPEN_CURLY;
  "}", CLOSE_CURLY;
  "(", OPEN_ROUND;
  ")", CLOSE_ROUND;
  ";", SEMICOLON;
  "int", KEYWORD_INT;
  "return", KEYWORD_RETURN;
  "[a-zA-Z]\\w*", IDENTIFIER;
  "[0-9]+*", INT_LITERAL;
]

let lex program = 
  if String.(program = "") then [] else []
