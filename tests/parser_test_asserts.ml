open Base
open Ast
open OUnit

let assert_strings_equal s1 s2 =
  assert_equal s1 s2 ~printer:Fn.id

let rec assert_equal_exps exp got = 
  match exp, got with
  | Const e, Const g -> assert_equal e g
  | Complement e, Complement g -> assert_equal_exps e g
  | Negation e, Negation g -> assert_equal_exps e g
  | Addition (e1, e2), Addition (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Subtraction (e1, e2), Subtraction (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Multiplication (e1, e2), Multiplication (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Division (e1, e2), Division (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Logical_Or (e1, e2), Logical_Or (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Logical_And (e1, e2), Logical_And (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Equality (e1, e2), Equality (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Inequality (e1, e2), Inequality (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | LessThan (e1, e2), LessThan (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | LessThanOrEqual (e1, e2), LessThanOrEqual (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | GreaterThan (e1, e2), GreaterThan (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | GreaterThanOrEqual (e1, e2), GreaterThanOrEqual (g1, g2) -> assert_equal_exps e1 g1; assert_equal_exps e2 g2
  | Assign (v1, e1), Assign (v2, e2) -> assert_strings_equal v1 v2; assert_equal_exps e1 e2
  | Var n1, Var n2 -> assert_strings_equal n1 n2
  | _, _ -> failwith (Printf.sprintf "expected %s but got %s" (show_exp exp) (show_exp got))

let assert_ok_exp (exp: Ast.exp) (got : (('a * Ast.exp), string) Result.t) _ctxt = 
  let got = snd (Result.ok_or_failwith got) in
  assert_equal_exps exp got
  
