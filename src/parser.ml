open Ast
open Base
open Tokens
open Parser_common
open Parser_exp
open Parser_stmt

let rec parse_block_items: block_item list tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  match List.hd tokens with
  | None -> Ok (tokens, [])
  | Some t when starts_statement_or_block_item t ->
      parse_block_item tokens >>= fun (tokens, block_item) ->
      parse_block_items tokens >>| fun (tokens, block_items) ->
      (tokens, block_item :: block_items)
  | Some _ -> Ok (tokens, [])

let parse_fundef: fundef tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_INT >>=
  parse_token_using_function (function | IDENTIFIER s -> Some s | _ -> None) "function name" >>= fun (tokens, func_name) ->
  parse_token OPEN_ROUND tokens >>=
  parse_token CLOSE_ROUND >>=
  parse_token OPEN_CURLY >>=
  parse_block_items >>= fun (tokens, body) ->
  parse_token CLOSE_CURLY tokens >>| fun tokens -> 
  (tokens, {name = func_name; body})

let parse tokens = 
  let open Result.Monad_infix in
  let error_if_unparsed (tokens, ast) =
    if List.is_empty tokens
    then Ok ast
    else Error "unparsed tokens at end" in
  parse_fundef tokens >>= error_if_unparsed