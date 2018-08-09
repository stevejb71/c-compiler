open Ast
open Base
open Tokens

type 'a parser = Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

let parse_token expected tokens =
  match List.hd tokens with
  | Some expected -> Ok (List.tl_exn tokens)
  | _ -> Error (Printf.sprintf "was expecting %s" (print_token expected))

let parse_exp: exp parser = fun tokens ->
  let open Result.Monad_infix in
  tokens |>
  parse_token INT_LITERAL >>= fun tokens ->
  Ok (tokens, Const 2)

let parse_return: stmt parser = fun tokens ->
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_RETURN >>=
  parse_exp >>= fun (tokens, exp) ->
  Ok (tokens, Return exp)

let parse_stmt: stmt parser = fun tokens -> 
  let open Result.Monad_infix in
  tokens |>
  parse_return >>= fun (tokens, ret) ->
  parse_token SEMICOLON tokens >>= fun tokens ->
  Ok (tokens, ret)

let parse_fundef: fundef parser = fun tokens -> 
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_INT >>=
  parse_token IDENTIFIER >>=
  parse_token OPEN_ROUND >>=
  parse_token CLOSE_ROUND >>=
  parse_token OPEN_CURLY >>=
  parse_stmt >>= fun (tokens, stmt) ->
  parse_token CLOSE_CURLY tokens >>= fun tokens -> 
  Ok (tokens, {name = "main"; body = stmt})

let parse tokens = 
  let open Result.Monad_infix in
  let error_if_unparsed (tokens, ast) =
    if List.is_empty tokens
    then Ok ast
    else Error "unparsed tokens at end" in
  parse_fundef tokens >>= error_if_unparsed