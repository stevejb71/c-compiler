open Base
open Re
open Tokens

let compile_tokens =
  let compile1 (str, tok) =
    let regex = Perl.re str |> Re.compile
    in (regex, tok)
  in
  List.map ~f:compile1

let token_regexs: (Re.re * Tokens.t) list = compile_tokens [
  "\\{", OPEN_CURLY;
  "\\}", CLOSE_CURLY;
  "\\(", OPEN_ROUND;
  "\\)", CLOSE_ROUND;
  ";", SEMICOLON;
  "int", KEYWORD_INT;
  "return", KEYWORD_RETURN;
  "[a-zA-Z]+", IDENTIFIER;
  "[0-9]+", INT_LITERAL;
]

let rec next_non_whitespace str pos =
  if pos = String.length str || not (Char.is_whitespace str.[pos])
  then pos
  else next_non_whitespace str (pos+1)

(** Finds the first token in token_regexs that matches str at the given pos, and returns the next pos + the token, in an option. *)
let match_token (str: string) (pos: int): ((int * Tokens.t), string) Result.t =
  let pos = next_non_whitespace str pos in
  let try1 regex = 
    match Re.exec_opt ~pos regex str with
    | None -> None
    | Some gs -> 
        if Group.test gs 0 && Group.start gs 0 = pos
        then Some (Re.Group.stop gs 0)
        else None
  in 
  let rec find = function
  | [] -> Error (Printf.sprintf "Nothing matches at position %d" pos)
  | (regex, token) :: tail -> 
      match try1 regex with
      | None -> find tail
      | Some p -> Ok (p, token)
      | exception _ -> Error (Printf.sprintf "Failure in regex at %d" pos)
  in
  find token_regexs

let strip_right str =
  if String.is_empty str then str
  else 
    let rec find_last_index_of_whitespace pos =
      if pos > 0 && Char.is_whitespace (str.[pos])
      then find_last_index_of_whitespace (pos - 1)
      else pos in
    let index = find_last_index_of_whitespace (String.length str - 1) in
    String.sub str ~pos:0 ~len:(index+1)
  
let lex program = 
  let program = strip_right program in
  let rec go pos acc =
    if pos = String.length program 
    then Ok acc
    else match match_token program pos with
    | Ok (pos, token) -> go pos (token :: acc)
    | Error msg -> Error msg
  in go 0 [] |> Result.map ~f:List.rev