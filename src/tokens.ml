type t =
  | OPEN_CURLY
  | CLOSE_CURLY
  | OPEN_ROUND
  | CLOSE_ROUND
  | SEMICOLON
  | KEYWORD_INT
  | KEYWORD_RETURN
  | IDENTIFIER
  | INT_LITERAL  

let print_token = function
| OPEN_CURLY -> "<OPEN_CURLY>"
| CLOSE_CURLY -> "<CLOSE_CURLY>"
| OPEN_ROUND -> "<OPEN_ROUND>"
| CLOSE_ROUND -> "<CLOSE_ROUND>"
| SEMICOLON -> "<SEMICOLON>"
| KEYWORD_INT -> "<KEYWORD_INT>"
| KEYWORD_RETURN -> "<KEYWORD_RETURN>"
| IDENTIFIER -> "<IDENTIFIER>"
| INT_LITERAL -> "<INT_LITERAL>"
  
  