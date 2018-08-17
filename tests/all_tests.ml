open OUnit2
open Asm_writer_tests
open Codegen_tests
open Lexer_tests
open Parser_tests
open Parser_exp_tests

let () =
  run_test_tt_main ("tests" >:::List.concat [
    asm_writer_tests;
    codegen_tests; 
    lexer_tests; 
    parser_tests;
    parser_exp_tests
  ])
