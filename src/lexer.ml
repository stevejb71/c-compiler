open Base
open Re
open Tokens
open Functions

let compile_tokens =
  let compile1 (str, tok) =
    let regex = Perl.re str |> Re.compile
    in (regex, tok)
  in
  List.map ~f:compile1

type token_maker = string -> int -> int -> (Tokens.t list, string) Result.t

let one a _ _ _ = Ok [a]

let parse_int: token_maker = fun str start finish ->
  let int_token = String.sub str start (finish - start) in
  let int_token = 
    try Ok (Int.of_string int_token) with
    | _ -> Error ("cannot read int from " ^ int_token) in
  Result.map int_token ~f:(fun n -> [INT_LITERAL n])

let parse_str: token_maker = fun str start finish ->
  Ok [IDENTIFIER (String.sub str start (finish - start))]

let token_regexs: (Re.re * token_maker) list = compile_tokens [
  "\\{", one OPEN_CURLY;
  "\\}", one CLOSE_CURLY;
  "\\(", one OPEN_ROUND;
  "\\)", one CLOSE_ROUND;
  ";", one SEMICOLON;
  "-", one NEGATION;
  "~", one COMPLEMENT;
  "!=", one NOT_EQUAL;
  "!", one LOGICAL_NEGATION;
  "\\+", one ADDITION;
  "\\*", one MULTIPLICATION;
  "/", one DIVISION;
  "&&", one LOGICAL_AND;
  "\\|\\|", one LOGICAL_OR;
  "==", one EQUAL;
  "<=", one LESS_THAN_OR_EQUAL;
  "<", one LESS_THAN;
  ">=", one GREATER_THAN_OR_EQUAL;
  ">", one GREATER_THAN;
  "int(\\s+)", one KEYWORD_INT;
  "return;", (fun _ _ _ -> Ok [KEYWORD_RETURN; SEMICOLON]);
  "return(\\s+)", one KEYWORD_RETURN;
  "[a-zA-Z]+", parse_str;
  "[0-9]+", parse_int;
]

let rec next_non_whitespace str pos =
  if pos = String.length str || not (Char.is_whitespace str.[pos])
  then pos
  else next_non_whitespace str (pos+1)

let safe_substr str ~pos ~len =
  let len = min len (String.length str - pos) in
  String.sub str ~pos ~len

(** Finds the first token in token_regexs that matches str at the given pos, and returns the next pos + the token, in an option. *)
let match_token (str: string) (pos: int): ((int * Tokens.t list), string) Result.t =
  let pos = next_non_whitespace str pos in
  let try1 regex =
    match Re.exec_opt ~pos regex str with
    | None -> None
    | Some gs -> 
        if Group.test gs 0 && Group.start gs 0 = pos
        then Some (Re.Group.stop gs 0)
        else None
  in 
  let rec find: (Re.re * token_maker) list -> ((int * token_maker), string) Result.t = function
  | [] -> Error (Printf.sprintf "No lexer token matching \"%s\" at position %d" (safe_substr str ~pos ~len:5) pos)
  | (regex, token_maker) :: tail -> 
      match try1 regex with
      | None -> find tail
      | Some p -> Ok (p, token_maker)
      | exception _ -> Error (Printf.sprintf "Failure in regex at %d" pos)
  in
  let open Result.Monad_infix in
  find token_regexs >>= fun (end_token, token_maker) -> 
  token_maker str pos end_token >>= fun t -> 
  Ok (end_token, t)

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
  in go 0 [] |> Result.map ~f:(List.concat >|> List.rev)