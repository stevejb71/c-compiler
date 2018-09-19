open Ast
open Base
open Tokens
open Parser_common

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
  let open Result.Monad_infix in
  match List.hd tokens with
  | Some (IDENTIFIER var) ->
      let parsed_assignment = 
        let tokens = List.tl_exn tokens in
        parse_token ASSIGNMENT tokens >>= fun tokens ->
        parse_exp tokens >>| fun (tokens, rhs) ->
        (tokens, Assign (var, rhs)) in
      if Result.is_ok parsed_assignment
      then parsed_assignment
      else parse_conditional_exp tokens
  | Some _ -> parse_conditional_exp tokens
  | None -> Error "no more tokens"

and parse_conditional_exp: exp tokens_parser = fun tokens ->
  let open Result.Monad_infix in
  parse_logical_or_exp tokens >>= fun (tokens, cond_exp) -> (
  match List.hd tokens with
  | Some QUESTION_MARK ->
      parse_token QUESTION_MARK tokens >>= 
      parse_exp >>= fun (tokens, if_true) ->
      parse_token COLON tokens >>= 
      parse_conditional_exp >>| fun (tokens, if_false) ->
      (tokens, ConditionalExp (cond_exp, if_true,if_false))
  | _ -> Ok (tokens, cond_exp)
  )

and parse_logical_or_exp: exp tokens_parser = fun tokens ->
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
  | Some (IDENTIFIER var) -> begin
      let tokens = List.tl_exn tokens in
      Ok (tokens, Var var)
    end
  | Some t -> Error (Printf.sprintf "parser error on %s" (print_token t))
  | None -> Error "no more tokens in parse_factor"
