open Base

type t =
  | OPEN_CURLY
  | CLOSE_CURLY
  | OPEN_ROUND
  | CLOSE_ROUND
  | SEMICOLON
  | KEYWORD_INT
  | KEYWORD_RETURN
  | IDENTIFIER
  | INT_LITERAL of int  
  | NEGATION
  | LOGICAL_NEGATION
  | COMPLEMENT
  | ADDITION
  | MULTIPLICATION
  | DIVISION

let print_token = function
| OPEN_CURLY -> "<OPEN_CURLY>"
| CLOSE_CURLY -> "<CLOSE_CURLY>"
| OPEN_ROUND -> "<OPEN_ROUND>"
| CLOSE_ROUND -> "<CLOSE_ROUND>"
| SEMICOLON -> "<SEMICOLON>"
| KEYWORD_INT -> "<KEYWORD_INT>"
| KEYWORD_RETURN -> "<KEYWORD_RETURN>"
| IDENTIFIER -> "<IDENTIFIER>"
| INT_LITERAL n -> Printf.sprintf "<INT_LITERAL: %d>" n
| NEGATION -> "<NEGATION>"
| LOGICAL_NEGATION -> "<LOGICAL_NEGATION>"
| COMPLEMENT -> "<COMPLEMENT>"
| ADDITION -> "<ADDITION>"
| MULTIPLICATION -> "<MULTIPLICATION>"
| DIVISION -> "<DIVISION>"
  
let eq (t1: t) (t2: t) = match t1, t2 with
| OPEN_CURLY, OPEN_CURLY -> true
| CLOSE_CURLY, CLOSE_CURLY -> true
| OPEN_ROUND, OPEN_ROUND -> true
| CLOSE_ROUND, CLOSE_ROUND -> true
| SEMICOLON, SEMICOLON -> true
| KEYWORD_INT, KEYWORD_INT -> true
| KEYWORD_RETURN, KEYWORD_RETURN -> true
| IDENTIFIER, IDENTIFIER -> true
| INT_LITERAL n1, INT_LITERAL n2 -> Int.(n1 = n2)
| NEGATION, NEGATION -> true
| LOGICAL_NEGATION, LOGICAL_NEGATION -> true
| COMPLEMENT, COMPLEMENT -> true
| ADDITION, ADDITION -> true
| MULTIPLICATION, MULTIPLICATION -> true
| DIVISION, DIVISION -> true
| _ -> false

let is_unary_op = function
| NEGATION | LOGICAL_NEGATION | COMPLEMENT -> true
| _ -> false
