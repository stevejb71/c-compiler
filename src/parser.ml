open Ast
open Base
open Tokens

type 'a tokens_parser = Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

let parse_token_using_function (accept: Tokens.t -> 'a) (expected: string) tokens =
  match List.hd tokens with
  | None -> Error (Printf.sprintf "was expecting %s but ran out of tokens" expected)
  | Some t ->
      match accept t with
      | None -> Error (Printf.sprintf "was expecting %s but got %s" expected (print_token t))
      | Some x -> Ok (List.tl_exn tokens, x)

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


let rec parse_stmts: stmt list tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  match List.hd tokens with
  | None -> Ok (tokens, [])
  | Some t when starts_statement t ->
      parse_stmt tokens >>= fun (tokens, stmt) ->
      parse_stmts tokens >>| fun (tokens, stmts) ->
      (tokens, stmt :: stmts)
  | Some _ -> Ok (tokens, [])

let parse_fundef: fundef tokens_parser = fun tokens -> 
  let open Result.Monad_infix in
  tokens |>
  parse_token KEYWORD_INT >>=
  parse_token_using_function (function | IDENTIFIER s -> Some s | _ -> None) "function name" >>= fun (tokens, func_name) ->
  parse_token OPEN_ROUND tokens >>=
  parse_token CLOSE_ROUND >>=
  parse_token OPEN_CURLY >>=
  parse_stmts >>= fun (tokens, body) ->
  parse_token CLOSE_CURLY tokens >>| fun tokens -> 
  (tokens, {name = func_name; body})

let parse tokens = 
  let open Result.Monad_infix in
  let error_if_unparsed (tokens, ast) =
    if List.is_empty tokens
    then Ok ast
    else Error "unparsed tokens at end" in
  parse_fundef tokens >>= error_if_unparsed