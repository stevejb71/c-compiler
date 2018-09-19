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

let parse_declare: block_item tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_INT >>=
  parse_token_using_function (function | IDENTIFIER s -> Some s | _ -> None) "variable name" >>= fun (tokens, variable_name) ->
  match List.hd tokens with
  | Some ASSIGNMENT ->
      let tokens = List.tl_exn tokens in
      parse_exp tokens >>= fun (tokens, e) ->
      parse_token SEMICOLON tokens >>| fun tokens ->
      (tokens, Declare {name=variable_name; initial_value=Some e})  
  | Some SEMICOLON -> 
      Ok (List.tl_exn tokens, Declare {name=variable_name; initial_value=None})      
  | _ -> Error "Expecting assignment or semicolon in a declaration"

let rec parse_stmt: stmt tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  match List.hd tokens with
  | None -> Error "no more tokens in parse_stmt"
  | Some KEYWORD_RETURN -> 
      parse_return tokens >>= fun (tokens, ret) ->
      parse_token SEMICOLON tokens >>| fun tokens ->
      (tokens, ret)
  (* | Some KEYWORD_INT ->
      parse_declare tokens >>| fun (tokens, ret) ->
      (tokens, ret) *)
  | Some KEYWORD_IF -> 
      let tokens = List.tl_exn tokens in
      parse_token OPEN_ROUND tokens >>= fun tokens ->
      parse_exp tokens >>= fun (tokens, cond) ->
      parse_token CLOSE_ROUND tokens >>= fun tokens ->
      parse_stmt tokens >>= fun (tokens, true_branch) -> (
        match List.hd tokens with
        | Some KEYWORD_ELSE -> (
            let tokens = List.tl_exn tokens in
            parse_stmt tokens >>| fun (tokens, false_branch) ->
            (tokens, Conditional (cond, true_branch, Some false_branch))
          )
        | _ -> Ok (tokens, Conditional (cond, true_branch, None))
      )
  | Some _ -> 
      parse_exp tokens >>= fun (tokens, e) ->
      parse_token SEMICOLON tokens >>| fun tokens ->
      (tokens, Exp e)

let parse_block_item: block_item tokens_parser = fun tokens -> 
  let stmt = parse_stmt tokens in
  if Result.is_ok stmt
  then stmt |> Result.map ~f:(fun (ts, stmt) -> (ts, Statement stmt))
  else parse_declare tokens