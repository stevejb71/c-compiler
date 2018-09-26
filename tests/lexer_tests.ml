open Core_kernel
open Lexer
open OUnit2
open Tokens
open Test_files

let assert_ok exp (got: (Tokens.t list, string) Result.t) _ctxt = 
  let got = Result.ok_or_failwith got in
  assert_equal exp got ~printer:(fun xs -> List.map ~f:print_token xs |> String.concat ~sep:",")

let assert_error (exp: string) (got: ('a, string) Result.t) _ctxt = 
  let got = Option.value_exn (Result.error got) in
  assert_equal exp got ~printer:Fn.id

let assert_can_lex_file foldername filename program =
  match lex program with
  | Ok _ -> ()
  | Error msg -> failwith (Printf.sprintf "Failed on '%s/%s' with msg %s" foldername filename msg)
  
let assert_can_lex_files_in_folder (foldername: string) _ctxt =
  for_each_file_in_folder ~foldername ~filter:(fun _ -> true) ~f:(assert_can_lex_file foldername)
  
let lexer_general_tests = [
  "empty string has no tokens" >::
    assert_ok [] (lex "");
  "single open curly bracket produces an OPEN_CURLY token" >::
    assert_ok [OPEN_CURLY] (lex "{");
  "single open round bracket preceded by whitespace produces an OPEN_ROUND token" >::
    assert_ok [OPEN_ROUND] (lex "      (");
  "single identifier followed by whitespace produces an IDENTIFIER token" >::
    assert_ok [IDENTIFIER "somefunc"] (lex "somefunc   ");
  "lexes more than one token" >::
    assert_ok [INT_LITERAL 7; IDENTIFIER "somefunc"; SEMICOLON] (lex "  7 somefunc  ; ");
  "lexes return0 as an identifier, not return, int" >::
    assert_ok [IDENTIFIER "return0"] (lex "return0");
  "lexes return 8 as RETURN + INT" >::
    assert_ok [KEYWORD_RETURN; INT_LITERAL 8] (lex "return 8");
  "lexes return; as RETURN + SEMICOLON" >::
    assert_ok [KEYWORD_RETURN; SEMICOLON] (lex "return;");
  "lexes int8; as IDENTIFIER int8" >::
    assert_ok [IDENTIFIER "int8"] (lex "int8");
  "reports an error on failing to lex " >::
    assert_error "Unexpected char \"#\" at position (1,8)" (lex "   doub#le");
  "lexes addition, multiplication, and division tokens" >::
    assert_ok [ADDITION; MULTIPLICATION; DIVISION] (lex "+ * /");
  "lexes stage 4 tokens" >::
    assert_ok 
      [LOGICAL_AND; LOGICAL_OR; EQUAL; LESS_THAN; GREATER_THAN; LESS_THAN_OR_EQUAL; GREATER_THAN_OR_EQUAL; NOT_EQUAL]
      (lex "&& || == < > <= >= !=");
  "lexes assignment" >::
    assert_ok [ASSIGNMENT] (lex "=");
  "lexes if statement" >::
    assert_ok [KEYWORD_IF; OPEN_ROUND; IDENTIFIER "x"; EQUAL; INT_LITERAL 5; CLOSE_ROUND; INT_LITERAL 7] (lex "if (x == 5) 7");
  "lexes if-else statement" >::
    assert_ok [KEYWORD_IF; OPEN_ROUND; IDENTIFIER "x"; EQUAL; INT_LITERAL 5; CLOSE_ROUND; INT_LITERAL 7; KEYWORD_ELSE; INT_LITERAL 8] (lex "if (x == 5) 7 else 8");
  "lexes ternary expression" >::
    assert_ok [OPEN_ROUND; IDENTIFIER "x"; EQUAL; INT_LITERAL 5; CLOSE_ROUND; QUESTION_MARK; INT_LITERAL 4; COLON; INT_LITERAL 9] (lex "(x == 5) ? 4 : 9");
]

let lexer_file_tests = [
  "lexes valid C stage 1 files" >::
    assert_can_lex_files_in_folder "stage_1/valid";
  "lexes invalid C stage 1 files" >::
    assert_can_lex_files_in_folder "stage_1/invalid";
  "lexes valid C stage 2 files" >::
    assert_can_lex_files_in_folder "stage_2/valid";
  "lexes invalid C stage 2 files" >::
    assert_can_lex_files_in_folder "stage_2/invalid";
  "lexes valid C stage 3 files" >::
    assert_can_lex_files_in_folder "stage_3/valid";
  "lexes invalid C stage 3 files" >::
    assert_can_lex_files_in_folder "stage_3/invalid";
  "lexes valid C stage 4 files" >::
    assert_can_lex_files_in_folder "stage_4/valid";
  "lexes invalid C stage 4 files" >::
    assert_can_lex_files_in_folder "stage_4/invalid";
  "lexes valid C stage 5 files" >::
    assert_can_lex_files_in_folder "stage_5/valid";
  "lexes invalid C stage 5 files" >::
    assert_can_lex_files_in_folder "stage_5/invalid";
]

let lexer_tests = List.concat [lexer_general_tests; lexer_file_tests]
