open OUnit2
open Lexer_tests
open Parser_tests

let () =
  run_test_tt_main ("tests" >:::List.concat [lexer_tests; parser_tests])
