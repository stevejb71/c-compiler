open Base
open Lexer_generator
open Lexing
open Tokens
open Functions
  
let lex program = 
  let buffer = Lexing.from_string program in
  let rec go acc =
    let token = Lexer_generator.read buffer in
    if Poly.(token <> Tokens.EOF)
    then go (token :: acc)
    else acc
  in 
  try 
    Ok (go [] |> List.rev)
  with
    Lexer_generator.SyntaxError msg -> Error msg
