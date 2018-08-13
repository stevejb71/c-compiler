(* 
Grammar accepted by this parser (stage 3):

<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <term> { ("+" | "-") <term> }
<term> ::= <factor> { ("*" | "/") <factor> }
<factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> 
*)

open Ast
open Base

val parse : Tokens.t list -> (program, string) Result.t

(** Included in mli for ease of testing *)
type 'a tokens_parser = Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

val parse_exp: exp tokens_parser
