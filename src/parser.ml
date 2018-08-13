open Ast
open Base
open Tokens

type 'a tokens_parser = Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

let parse_token (expected: Tokens.t) (tokens: Tokens.t list): (Tokens.t list, string) Result.t =
  match List.hd tokens with
  | Some t when Tokens.eq t expected -> Ok (List.tl_exn tokens)
  | Some t -> Error (Printf.sprintf "was expecting %s but got %s" (print_token expected) (print_token t))
  | None -> Error "no more tokens"

let unary_op_ast exp = function
| COMPLEMENT -> Complement exp
| NEGATION -> Negation exp
| LOGICAL_NEGATION -> Logical_Negation exp
| _ -> failwith "bug"

let rec parse_factor: exp tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  match List.hd tokens with
  | Some t when Tokens.eq t OPEN_ROUND ->
      tokens |>
      parse_token OPEN_ROUND >>=
      parse_exp >>= fun (tokens, exp) ->
      parse_token CLOSE_ROUND tokens >>| fun tokens ->
      (tokens, exp)
  | Some t when is_unary_op t ->
      List.tl_exn tokens |>
      parse_factor >>| fun (tokens, exp) ->
      (tokens, unary_op_ast exp t)
  | Some (INT_LITERAL n) ->
      Ok (List.tl_exn tokens, Const n)
  | Some _ -> Error "parser error"
  | _ -> Error "no more tokens"

and parse_term: exp tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  let rec parse_factors exp: exp tokens_parser = fun tokens ->
    match List.hd tokens with
    | Some MULTIPLICATION -> 
      List.tl_exn tokens |>
      parse_factor >>= fun (tokens, e2) ->
      parse_factors (Multiplication (exp, e2)) tokens
    | Some DIVISION -> 
      List.tl_exn tokens |>
      parse_factor >>= fun (tokens, e2) ->
      parse_factors (Division (exp, e2)) tokens
    | _ -> Ok (tokens, exp)
  in
  tokens |>
  parse_factor >>= fun (tokens, e1) ->
  parse_factors e1 tokens >>| fun (tokens, e) ->
  (tokens, e)

and parse_exp: exp tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  let rec parse_terms exp: exp tokens_parser = fun tokens ->
    match List.hd tokens with
    | Some ADDITION -> 
      List.tl_exn tokens |>
      parse_term >>= fun (tokens, e2) ->
      parse_terms (Addition (exp, e2)) tokens
    | Some NEGATION -> 
      List.tl_exn tokens |>
      parse_term >>= fun (tokens, e2) ->
      parse_terms (Subtraction (exp, e2)) tokens
    | _ -> Ok (tokens, exp)
  in
  tokens |>
  parse_term >>= fun (tokens, e1) ->
  parse_terms e1 tokens >>| fun (tokens, e) ->
  (tokens, e)

let parse_return: stmt tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_RETURN >>=
  parse_exp >>| fun (tokens, exp) ->
  (tokens, Return exp)

let parse_stmt: stmt tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  tokens |>
  parse_return >>= fun (tokens, ret) ->
  parse_token SEMICOLON tokens >>| fun tokens ->
  (tokens, ret)

let parse_fundef: fundef tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_INT >>=
  parse_token IDENTIFIER >>=
  parse_token OPEN_ROUND >>=
  parse_token CLOSE_ROUND >>=
  parse_token OPEN_CURLY >>=
  parse_stmt >>= fun (tokens, stmt) ->
  parse_token CLOSE_CURLY tokens >>| fun tokens -> 
  (tokens, {name = "main"; body = stmt})

let parse tokens = 
  let open Result.Monad_infix in
  let error_if_unparsed (tokens, ast) =
    if List.is_empty tokens
    then Ok ast
    else Error "unparsed tokens at end" in
  parse_fundef tokens >>= error_if_unparsed