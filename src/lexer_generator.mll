{
open Lexing
open Parser
open Tokens

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT_LITERAL (int_of_string (Lexing.lexeme lexbuf)) }
  | '{'      { OPEN_CURLY }
  | '}'      { CLOSE_CURLY }
  | '('      { OPEN_ROUND }
  | ')'      { CLOSE_ROUND }
  | '-'      { NEGATION }
  | '~'      { COMPLEMENT }
  | "!="     { NOT_EQUAL }
  | '!'      { LOGICAL_NEGATION }
  | '+'      { ADDITION }
  | '*'      { MULTIPLICATION }
  | '/'      { DIVISION }
  | "&&"     { LOGICAL_AND }
  | "||"     { LOGICAL_OR }
  | "=="     { EQUAL }
  | '='      { ASSIGNMENT }
  | "<="     { LESS_THAN_OR_EQUAL }
  | '<'      { LESS_THAN }
  | ">="     { GREATER_THAN_OR_EQUAL }
  | '>'      { GREATER_THAN }
  | "if"     { KEYWORD_IF }
  | "else"   { KEYWORD_ELSE }
  | '?'      { QUESTION_MARK }
  | ':'      { COLON }
  | "int"    { KEYWORD_INT }
  | "return" { KEYWORD_RETURN }
  | ';'      { SEMICOLON }
  | id       { IDENTIFIER (Lexing.lexeme lexbuf) }
  | eof      { EOF }
  | _ { raise @@ SyntaxError (Printf.sprintf "Unexpected char \"%s\" at position (%d,%d)" 
                               (Lexing.lexeme lexbuf) lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_pos) }
