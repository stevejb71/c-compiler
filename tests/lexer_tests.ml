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

let assert_can_lex_file filename program =
  match lex program with
  | Ok _ -> ()
  | Error msg -> failwith (Printf.sprintf "Failed on '%s' with msg %s" filename msg)
  
let assert_can_lex_files_in_folder (foldername: string) _ctxt =
  for_each_file_in_folder ~foldername ~f:assert_can_lex_file
  
let lexer_tests = [
  "empty string has no tokens" >::
    assert_ok [] (lex "");
  "single open curly bracket produces an OPEN_CURLY token" >::
    assert_ok [OPEN_CURLY] (lex "{");
  "single open round bracket preceded by whitespace produces an OPEN_ROUND token" >::
    assert_ok [OPEN_ROUND] (lex "      (");
  "single identifier followed by whitespace produces an IDENTIFIER token" >::
    assert_ok [IDENTIFIER] (lex "somefunc   ");
  "lexes more than one token" >::
    assert_ok [INT_LITERAL 7; IDENTIFIER; SEMICOLON] (lex "  7 somefunc  ; ");
  "lexes return0 as an identifier, not return, int" >::
    assert_ok [IDENTIFIER; INT_LITERAL 0] (lex "return0");
  "lexes return 8 as RETURN + INT" >::
    assert_ok [KEYWORD_RETURN; INT_LITERAL 8] (lex "return 8");
  "lexes return; as RETURN + SEMICOLON" >::
    assert_ok [KEYWORD_RETURN; SEMICOLON] (lex "return;");
  "lexes int8; as IDENTIFIER + INT" >::
    assert_ok [IDENTIFIER; INT_LITERAL 8] (lex "int8");
  "reports an error on failing to lex " >::
    assert_error "Nothing matches at position 7" (lex "   doub:le");
  "lexes addition, multiplication, and division tokens" >::
    assert_ok [ADDITION; MULTIPLICATION; DIVISION] (lex "+ * /");
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
]