open OUnit2
open Codegen_tests
open Lexer_tests
open Parser_tests

let () =
  run_test_tt_main ("tests" >:::List.concat [
    codegen_tests; 
    lexer_tests; 
    parser_tests
  ])
