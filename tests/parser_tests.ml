open Base
open OUnit2
open Ast
open Tokens
open Parser 
open Lexer
open Test_files

let assert_ok exp got _ctxt =
  let got = Result.ok_or_failwith got in
  assert_equal (Ast.show_program exp) (Ast.show_program got) ~printer:Fn.id

let assert_ok_exp exp got _ctxt = 
  let got = snd (Result.ok_or_failwith got) in
  let rec go exp got = 
    match exp, got with
    | Const ne, Const ng -> assert_equal ne ng
    | Complement e1, Complement e2 -> go e1 e2
    | Negation e1, Negation e2 -> go e1 e2
    | _ -> failwith "test assertion not implemented"
  in go exp got
  
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
    
let parser_tests = [
  "empty list is an error" >::
    assert_error "no more tokens" (parse []);
  "parses the stage 1 C function" >::
    assert_ok {name="main"; body=Return (Const 52)} @@ parse [KEYWORD_INT; IDENTIFIER; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; KEYWORD_RETURN; INT_LITERAL 52; SEMICOLON; CLOSE_CURLY];
  "spots an error if stage 1 C function has badly placed brackets" >::
    assert_error "was expecting <OPEN_ROUND> but got <OPEN_CURLY>" @@ parse [KEYWORD_INT; IDENTIFIER; OPEN_CURLY; CLOSE_CURLY; OPEN_CURLY; KEYWORD_RETURN; INT_LITERAL 3; SEMICOLON; CLOSE_CURLY];
  "parse_exp parses an int expression" >::
    assert_ok_exp (Const 42) @@ parse_exp [INT_LITERAL 42];
  "parse_exp parses a bitwise negated int expression" >::
    assert_ok_exp (Complement (Const 42)) @@ parse_exp [COMPLEMENT; INT_LITERAL 42];
  "parse_exp parses a negated int expression" >::
    assert_ok_exp (Negation (Const 5)) @@ parse_exp [NEGATION; INT_LITERAL 5];
  "lexes and parses valid stage 1 C files" >::
    assert_can_parse_files_in_folder "stage_1/valid";
  "lexes but does not parse invalid stage 1 C files" >::
    assert_fails_to_parse_files_in_folder "stage_1/invalid";
  "lexes and parses valid stage 2 C files" >::
    assert_can_parse_files_in_folder "stage_2/valid";
  "lexes but does not parse invalid stage 2 C files" >::
    assert_fails_to_parse_files_in_folder "stage_2/invalid";
]