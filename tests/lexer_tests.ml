open Core_kernel
open Lexer
open OUnit2
open Tokens

let assert_ok exp (got: (Tokens.t list, string) Result.t) _ctxt = 
  let got = Result.ok_or_failwith got in
  assert_equal exp got ~printer:(fun xs -> List.map ~f:print_token xs |> String.concat ~sep:",")

let assert_error (exp: string) (got: ('a, string) Result.t) _ctxt = 
  let got = Option.value_exn (Result.error got) in
  assert_equal exp got ~printer:Fn.id

let assert_can_lex_file (filename: string) _ctxt =
  let program = In_channel.read_all filename in
  match lex program with
  | Ok _ -> ()
  | Error msg -> failwith ("Failed: " ^ msg)
  
let assert_can_lex_files_in_folder (foldername: string) _ctxt =
  let foldername = "../../../tests/" ^ foldername in
  let c_files = Sys.readdir foldername in
  Array.iter c_files ~f:(fun name -> assert_can_lex_file (foldername ^ "/" ^ name) _ctxt)
  
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
    assert_ok [INT_LITERAL; IDENTIFIER; SEMICOLON] (lex "  7 somefunc  ; ");
  "reports an error on failing to lex " >::
    assert_error "Nothing matches at position 7" (lex "   doub:le");
  "lexes valid C files" >::
    assert_can_lex_files_in_folder "stage_1/valid";
  "lexes invalid C files" >::
    assert_can_lex_files_in_folder "stage_1/invalid";
]