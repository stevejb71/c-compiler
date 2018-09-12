open Ast
open Base
open Tokens

type 'a tokens_parser = Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

let parse_token_using_function (accept: Tokens.t -> 'a option) (expected: string) (tokens: Tokens.t list): ((Tokens.t list * 'a), string) Result.t =
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
  | None -> Error (Printf.sprintf "was expecting %s but no more tokens" (print_token expected))
      
      