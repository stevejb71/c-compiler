open Base
open OUnit2
open Ast
open Tokens
open Parser 

let assert_ok exp got _ctxt =
  let got = Result.ok_or_failwith got in
  assert_equal (Ast.show_program exp) (Ast.show_program got) ~printer:Fn.id

let assert_error (exp: string) (got: ('a, string) Result.t) _ctxt = 
  let got = Option.value_exn (Result.error got) in
  assert_equal exp got ~printer:Fn.id

let parser_tests = [
  "empty list is an error" >::
    assert_error "was expecting <KEYWORD_INT>" (parse []);
  "parses the stage 1 C function" >:: fun ctxt ->
    let actual = parse [KEYWORD_INT; IDENTIFIER; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; KEYWORD_RETURN; INT_LITERAL; SEMICOLON; CLOSE_CURLY] in
    assert_ok {name="main"; body=Return (Const 2)} actual ctxt
]