(* 
Grammar accepted by this parser (stage 4):

<program> ::= <function>
<statement> ::= "return" <exp> ";" | <exp> ";" | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
<declaration> ::= "int" <id> [ = <exp> ] ";"
<block-item> ::= <statement> | <declaration>
<function> ::= "int" <id> "(" ")" "{" { <block-item> } "}"
<exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
<logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
<equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
<relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
<additive-exp> ::= <term> { ("+" | "-") <term> }
<term> ::= <factor> { ("*" | "/") <factor> }
<factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
*)

open Ast
open Base
open Parser_common

val parse: Tokens.t list -> (program, string) Result.t