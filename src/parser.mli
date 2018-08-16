(* 
Grammar accepted by this parser (stage 4):

<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
<logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
<equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
<relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
<additive-exp> ::= <term> { ("+" | "-") <term> }
<term> ::= <factor> { ("*" | "/") <factor> }
<factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> 
*)

open Ast
open Base

val parse: Tokens.t list -> (program, string) Result.t

(** Included in mli for ease of testing *)
type 'a tokens_parser = Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

val parse_exp: exp tokens_parser

val parse_stmt: stmt tokens_parser
