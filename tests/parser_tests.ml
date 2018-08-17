open Base
open OUnit2
open Ast
open Tokens
open Parser 
open Lexer
open Test_files
open Parser_stmt

let assert_ok exp got _ctxt =
  let got = Result.ok_or_failwith got in
  assert_equal (Ast.show_program exp) (Ast.show_program got) ~printer:Fn.id

let assert_ok_stmt stmt got _ctxt = 
  let got = snd (Result.ok_or_failwith got) in
  let rec go assert_ok_stmt got = 
    match stmt, got with
    | Declare {name=n1; initial_value=v1}, Declare {name=n2; initial_value=v2} -> assert_equal n1 n2 ~printer:Fn.id; 
    | Return e1, Return e2 -> failwith "laters"
    | _, _ -> failwith "laters"
  in go stmt got
  
let assert_error (exp: string) (got: ('a, string) Result.t) _ctxt = 
  match got with
  | Ok _ -> failwith "Was expecting a failure"
  | Error msg -> assert_equal exp msg ~printer:Fn.id

let assert_can_parse_file filename program =
  let open Result.Monad_infix in
  let lexed = lex program |> Result.ok_or_failwith in
  match parse lexed with
  | Ok _ -> ()
  | Error msg -> failwith (Printf.sprintf "Failed on '%s' with msg %s" filename msg)
  
let assert_fails_to_parse_file filename program =
  let open Result.Monad_infix in
  let lexed = lex program |> Result.ok_or_failwith in
  match parse lexed with
  | Ok _ -> failwith (Printf.sprintf "Should have failed on '%s'" filename)
  | Error msg -> ()
  
let assert_can_parse_files_in_folder (foldername: string) _ctxt =
  for_each_file_in_folder ~foldername ~f:assert_can_parse_file

let assert_fails_to_parse_files_in_folder (foldername: string) _ctxt =
  for_each_file_in_folder ~foldername ~f:assert_fails_to_parse_file
    
let statement_tests = [
  "a statement can be a declaration without initializer" >::
    assert_ok_stmt (Declare {name="x"; initial_value=None}) (parse_stmt [KEYWORD_INT; IDENTIFIER "x"; SEMICOLON;]);
]
  
let general_parser_tests = [
  "empty list is an error" >::
    assert_error "no more tokens" (parse []);
  "expects a function name after int on its own" >::
    assert_error "was expecting function name but ran out of tokens" (parse [KEYWORD_INT]);
  "parses the stage 1 C function" >::
    assert_ok {name="main"; body=[Return (Const 52)]} @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; KEYWORD_RETURN; INT_LITERAL 52; SEMICOLON; CLOSE_CURLY];
  "spots an error if stage 1 C function has badly placed brackets" >::
    assert_error "was expecting <OPEN_ROUND> but got <OPEN_CURLY>" @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_CURLY; CLOSE_CURLY; OPEN_CURLY; KEYWORD_RETURN; INT_LITERAL 3; SEMICOLON; CLOSE_CURLY];
  "a function body can be empty" >::
    assert_ok {name="main"; body=[]} @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; CLOSE_CURLY];
  "a function body can have two statements" >::
    assert_ok {name="main"; body=[Return (Const 1); Return (Const 2)]} @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; 
      KEYWORD_RETURN; INT_LITERAL 1; SEMICOLON; KEYWORD_RETURN; INT_LITERAL 2; SEMICOLON; CLOSE_CURLY];
]

let parser_file_tests = [
  "lexes and parses valid stage 1 C files" >::
    assert_can_parse_files_in_folder "stage_1/valid";
  "lexes but does not parse invalid stage 1 C files" >::
    assert_fails_to_parse_files_in_folder "stage_1/invalid";
  "lexes and parses valid stage 2 C files" >::
    assert_can_parse_files_in_folder "stage_2/valid";
  "lexes but does not parse invalid stage 2 C files" >::
    assert_fails_to_parse_files_in_folder "stage_2/invalid";
  "lexes and parses valid stage 3 C files" >::
    assert_can_parse_files_in_folder "stage_3/valid";
  "lexes but does not parse invalid stage 3 C files" >::
    assert_fails_to_parse_files_in_folder "stage_3/invalid";
  "lexes and parses valid stage 4 C files" >::
    assert_can_parse_files_in_folder "stage_4/valid";
  "lexes but does not parse invalid stage 4 C files" >::
    assert_fails_to_parse_files_in_folder "stage_4/invalid";
]

let parser_tests = List.concat [general_parser_tests; parser_file_tests; statement_tests]