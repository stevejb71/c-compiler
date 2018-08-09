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

val lex : string -> (token list, string) Result.t