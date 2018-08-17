open Base
open OUnit2
open Ast
open Tokens
open Lexer
open Test_files
open Parser_exp
open Parser_test_asserts

let assert_ok exp got _ctxt =
  let got = Result.ok_or_failwith got in
  assert_equal (Ast.show_program exp) (Ast.show_program got) ~printer:Fn.id

let assert_error (exp: string) (got: ('a, string) Result.t) _ctxt = 
  match got with
  | Ok _ -> failwith "Was expecting a failure"
  | Error msg -> assert_equal exp msg ~printer:Fn.id

let parser_exp_tests = [
  "parse_exp parses an int expression" >::
    assert_ok_exp (Const 42) @@ parse_exp [INT_LITERAL 42];
  "parse_exp parses a bitwise negated int expression" >::
    assert_ok_exp (Complement (Const 42)) @@ parse_exp [COMPLEMENT; INT_LITERAL 42];
  "parse_exp parses a negated int expression" >::
    assert_ok_exp (Negation (Const 5)) @@ parse_exp [NEGATION; INT_LITERAL 5];
  "parse_exp parses an addition" >::
    assert_ok_exp (Addition (Const 12, Const 5)) @@ parse_exp [INT_LITERAL 12; ADDITION; INT_LITERAL 5];
  "parse_exp parses a multiplication" >::
    assert_ok_exp (Multiplication (Const 12, Const 5)) @@ parse_exp [INT_LITERAL 12; MULTIPLICATION; INT_LITERAL 5];
  "parse_exp parses a subtraction using the NEGATION symbol" >::
    assert_ok_exp (Subtraction (Const 12, Const 5)) @@ parse_exp [INT_LITERAL 12; NEGATION; INT_LITERAL 5];
  "parse_exp parses a division" >::
    assert_ok_exp (Division (Const 12, Const 5)) @@ parse_exp [INT_LITERAL 12; DIVISION; INT_LITERAL 5];
  "parse_exp parses multiplication with precedence over addition" >::
    assert_ok_exp (Addition ((Const 1),(Multiplication (Const 12, Const 5)))) @@ parse_exp [INT_LITERAL 1; ADDITION; INT_LITERAL 12; MULTIPLICATION; INT_LITERAL 5];
  "parse_exp parses addition in brackets with precedence over multiplication" >::
    assert_ok_exp (Multiplication (Addition (Const 1 ,Const 12), Const 5)) @@ parse_exp [OPEN_ROUND; INT_LITERAL 1; ADDITION; INT_LITERAL 12; CLOSE_ROUND; MULTIPLICATION; INT_LITERAL 5];
  "parse_exp parses subtraction left associatively" >::
    assert_ok_exp (Subtraction (Subtraction (Const 1, Const 2), Const 3)) @@ parse_exp [INT_LITERAL 1; NEGATION; INT_LITERAL 2; NEGATION; INT_LITERAL 3];
  "parse_exp parses || with lower precedence than &&" >::
    assert_ok_exp (Logical_Or (Const 12, Logical_And (Const 5, Const 7))) @@ parse_exp [INT_LITERAL 12; LOGICAL_OR; INT_LITERAL 5; LOGICAL_AND; INT_LITERAL 7];
  "parse_exp an assigment" >::
    assert_ok_exp (Assign ("X", Const 5)) @@ parse_exp [IDENTIFIER "X"; ASSIGNMENT; INT_LITERAL 5;];
]
