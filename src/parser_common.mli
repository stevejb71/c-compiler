open Ast
open Base
open Tokens

type 'a tokens_parser = Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

val parse_token_using_function: (Tokens.t -> 'a option) -> string -> Tokens.t list -> ((Tokens.t list * 'a), string) Result.t

val parse_token: Tokens.t -> Tokens.t list -> (Tokens.t list, string) Result.t