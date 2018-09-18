
open Base
open OUnit2
open Ast
open Codegen_rules

let are_equal_stmts s1 s2 = String.(show_stmt s1 = show_stmt s2)

let are_eq_fundefs {name=exp_name; body=exp_body} {name=got_name; body=got_body} =
  if String.(exp_name <> got_name) then false
  else List.equal exp_body got_body ~equal:are_equal_stmts

let assert_eq_fundefs exp got _ctxt =
  let error_msg = Printf.sprintf "exp='%s'\ngot='%s'" (show_program exp) (show_program got) in
  assert_bool error_msg (are_eq_fundefs exp got)

let codegen_rules_tests = [
  "function that is not main does not have return appended" >:: (
    let f = {name="some_func"; body=[]} in
    let gen = generate_return f in
    assert_eq_fundefs f gen           
  );
  "main function with just return does not have return appended" >:: (
    let f = {name="main"; body=[Return (Const 10)]} in
    let gen = generate_return f in
    assert_eq_fundefs f gen           
  );
  "main function with conditional, each branching ending in return, does not have return appended" >:: (
    let return_stmt = Return (Const 10) in
    let f = {name="main"; body=[Conditional ((Const 5), return_stmt, Some return_stmt)]} in
    let gen = generate_return f in
    assert_eq_fundefs f gen           
  );
  "main function with no return has return appended" >:: (
    let exp = {name="main"; body=[Return (Const 0)]} in
    let gen = generate_return {name="main"; body=[]} in
    assert_eq_fundefs exp gen           
  );
]