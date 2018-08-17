open Ast
open Base
open Tokens
open Parser_common
open Parser_exp

let parse_return: stmt tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_RETURN >>=
  parse_exp >>| fun (tokens, exp) ->
  (tokens, Return exp)

let parse_declare: stmt tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_INT >>=
  parse_token_using_function (function | IDENTIFIER s -> Some s | _ -> None) "variable name" >>| fun (tokens, variable_name) ->
  (tokens, Declare {name=variable_name; initial_value=None})

let parse_stmt: stmt tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  match List.hd tokens with
  | None -> Error "no more tokens"
  | Some KEYWORD_RETURN -> 
      parse_return tokens >>= fun (tokens, ret) ->
      parse_token SEMICOLON tokens >>| fun tokens ->
      (tokens, ret)
  | Some KEYWORD_INT ->
      parse_declare tokens >>= fun (tokens, ret) ->
      parse_token SEMICOLON tokens >>| fun tokens ->
      (tokens, ret)
  | Some _ -> Error "not a statement"