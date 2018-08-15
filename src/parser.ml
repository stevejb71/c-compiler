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

let parse_many2 parse_exp token_to_ast_maker: exp tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  let rec parse_loop exp: exp tokens_parser = fun tokens ->
    match List.hd tokens with
    | None -> Ok (tokens, exp)
    | Some t ->
        match token_to_ast_maker t with
        | None -> Ok (tokens, exp)
        | Some make_ast ->
            List.tl_exn tokens |>
            parse_exp >>= fun (tokens, e2) ->
            parse_loop (make_ast exp e2) tokens
  in
  tokens |>
  parse_exp >>= fun (tokens, e1) ->
  parse_loop e1 tokens >>| fun (tokens, e) ->
  (tokens, e)
            
let parse_many parse_exp token make_ast: exp tokens_parser = fun tokens ->
  parse_many2 
    parse_exp
    (fun t -> if Tokens.eq t token then Some make_ast else None)
    tokens

let rec parse_exp: exp tokens_parser = fun tokens ->
  parse_many 
    parse_logical_and_exp 
    LOGICAL_OR 
    (fun e1 e2 -> Logical_Or (e1, e2))
    tokens

and parse_logical_and_exp: exp tokens_parser = fun tokens ->
  parse_many 
    parse_equality_exp 
    LOGICAL_AND 
    (fun e1 e2 -> Logical_And (e1, e2))
    tokens

and parse_equality_exp: exp tokens_parser = fun tokens ->
  let tokens_to_ast_maker = function
  | EQUAL -> Some (fun e1 e2 -> Equality (e1, e2))
  | NOT_EQUAL -> Some (fun e1 e2 -> Inequality (e1, e2)) 
  | _ -> None in
  parse_many2 parse_relational_exp tokens_to_ast_maker tokens

and parse_relational_exp: exp tokens_parser = fun tokens ->
  let tokens_to_ast_maker = function
  | LESS_THAN -> Some (fun e1 e2 -> LessThan (e1, e2))
  | GREATER_THAN -> Some (fun e1 e2 -> GreaterThan (e1, e2)) 
  | LESS_THAN_OR_EQUAL -> Some (fun e1 e2 -> LessThanOrEqual (e1, e2)) 
  | GREATER_THAN_OR_EQUAL -> Some (fun e1 e2 -> GreaterThanOrEqual (e1, e2)) 
  | _ -> None in
  parse_many2 parse_additive_exp tokens_to_ast_maker tokens

and parse_additive_exp: exp tokens_parser = fun tokens ->
  let tokens_to_ast_maker = function
  | ADDITION -> Some (fun e1 e2 -> Addition (e1, e2))
  | NEGATION -> Some (fun e1 e2 -> Subtraction (e1, e2)) 
  | _ -> None in
  parse_many2 parse_term tokens_to_ast_maker tokens

and parse_term: exp tokens_parser = fun tokens ->
  let tokens_to_ast_maker = function
  | MULTIPLICATION -> Some (fun e1 e2 -> Multiplication (e1, e2))
  | DIVISION -> Some (fun e1 e2 -> Division (e1, e2)) 
  | _ -> None in
  parse_many2 parse_factor tokens_to_ast_maker tokens

and parse_factor: exp tokens_parser = fun tokens ->
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