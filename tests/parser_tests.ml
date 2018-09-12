open Base
open OUnit2
open Ast
open Tokens
open Parser 
open Lexer
open Test_files
open Parser_stmt
open Parser_test_asserts

let assert_ok exp got _ctxt =
  let got = Result.ok_or_failwith got in
  assert_equal (Ast.show_program exp) (Ast.show_program got) ~printer:Fn.id

let rec assert_equal_stmts exp got = 
  match exp, got with
  | Declare {name=n1; initial_value=e1}, Declare {name=n2; initial_value=e2} -> begin
      assert_equal n1 n2 ~printer:Fn.id; 
      match e1, e2 with
      | Some e1, Some e2 -> assert_equal_exps e1 e2;
      | None, None -> ()
      | _ -> failwith "no match"
    end
  | Exp e1, Exp e2 -> assert_equal_exps e1 e2;
  | Return e1, Return e2 -> assert_equal_exps e1 e2;
  | Conditional (c1,t1,f1), Conditional (c2,t2,f2) -> begin
      assert_equal_exps c1 c2;
      assert_equal_stmts t1 t2;
      match f1,f2 with
      | Some f1,Some f2 -> assert_equal_stmts f1 f2;
      | None,None -> ()
      | _ -> failwith "mismatch on false branches";
    end
  | _ -> failwith "either no match or missing cases!"
  
let assert_ok_stmt stmt got _ctxt = 
  let (remaining, got) = Result.ok_or_failwith got in
  assert_bool "not all tokens parsed" (List.is_empty remaining);
  assert_equal_stmts stmt got
    
let assert_error (exp: string) (got: ('a, string) Result.t) _ctxt = 
  match got with
  | Ok _ -> failwith "Was expecting a failure"
  | Error msg -> assert_equal exp msg ~printer:Fn.id

let assert_can_parse_file filename program =
  let open Result.Monad_infix in
  let lexed = lex program |> Result.ok_or_failwith in
  match parse lexed with
  | Ok _ -> ()
  | Error msg -> failwith (Printf.sprintf "Failed on '%s' with msg %s" filename msg)
  
let assert_fails_to_parse_file filename program  =
  let open Result.Monad_infix in
  let lexed = lex program |> Result.ok_or_failwith in
  match parse lexed with
  | Ok _ -> failwith (Printf.sprintf "Should have failed on '%s'" filename)
  | Error msg -> ()
  
let assert_can_parse_files_in_folder (foldername: string) ?filter:(filter=(fun _ -> true)) _ctxt  =
  for_each_file_in_folder ~foldername ~f:assert_can_parse_file ~filter

let assert_fails_to_parse_files_in_folder (foldername: string) ?filter:(filter=(fun _ -> true)) _ctxt =
  for_each_file_in_folder ~foldername ~f:assert_fails_to_parse_file ~filter
    
let statement_tests = [
  "a statement can be a declaration without initializer" >::
    assert_ok_stmt (Declare {name="x"; initial_value=None}) (parse_stmt [KEYWORD_INT; IDENTIFIER "x"; SEMICOLON;]);
  "a statement can be a declaration with an initializer" >::
    assert_ok_stmt (Declare {name="x"; initial_value=Some (Const 10)}) (parse_stmt [KEYWORD_INT; IDENTIFIER "x"; ASSIGNMENT; INT_LITERAL 10; SEMICOLON]);
  "a standalone expression is a statement" >::
    assert_ok_stmt (Exp (Addition (Const 2, Const 4))) (parse_stmt [INT_LITERAL 2; ADDITION; INT_LITERAL 4; SEMICOLON]);
  "can parse an if statement without else" >:: (
    let condition = Const 10 in
    let true_branch = Exp (Assign ("x", Const 11)) in
    let tokens = [KEYWORD_IF; OPEN_ROUND; INT_LITERAL 10; CLOSE_ROUND; IDENTIFIER "x"; ASSIGNMENT; INT_LITERAL 11; SEMICOLON;] in
    assert_ok_stmt (Conditional (condition, true_branch, None)) (parse_stmt tokens);
  );
  "can parse an if statement with else branch" >:: (
    let condition = Const 10 in
    let true_branch = Exp (Assign ("x", Const 11)) in
    let false_branch = Exp (Assign ("y", Const 8)) in
    let tokens = [KEYWORD_IF; OPEN_ROUND; INT_LITERAL 10; CLOSE_ROUND; 
                    IDENTIFIER "x"; ASSIGNMENT; INT_LITERAL 11; SEMICOLON;
                  KEYWORD_ELSE; 
                    IDENTIFIER "y"; ASSIGNMENT; INT_LITERAL 8; SEMICOLON;
                 ] in
    assert_ok_stmt (Conditional (condition, true_branch, Some false_branch)) (parse_stmt tokens);
  );
]
  
let general_parser_tests = [
  "empty list is an error" >::
    assert_error "was expecting <KEYWORD_INT> but no more tokens" (parse []);
  "expects a function name after int on its own" >::
    assert_error "was expecting function name but ran out of tokens" (parse [KEYWORD_INT]);
  "parses the stage 1 C function" >::
    assert_ok {name="main"; body=[Return (Const 52)]} @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; KEYWORD_RETURN; INT_LITERAL 52; SEMICOLON; CLOSE_CURLY];
  "spots an error if stage 1 C function has badly placed brackets" >::
    assert_error "was expecting <OPEN_ROUND> but got <OPEN_CURLY>" @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_CURLY; CLOSE_CURLY; OPEN_CURLY; KEYWORD_RETURN; INT_LITERAL 3; SEMICOLON; CLOSE_CURLY];
  "a function body can be empty" >::
    assert_ok {name="main"; body=[]} @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; CLOSE_CURLY];
  "a function body can have two statements" >::
    assert_ok {name="main"; body=[Return (Const 1); Return (Const 2)]} @@ parse [KEYWORD_INT; IDENTIFIER "main"; OPEN_ROUND; CLOSE_ROUND; OPEN_CURLY; 
      KEYWORD_RETURN; INT_LITERAL 1; SEMICOLON; KEYWORD_RETURN; INT_LITERAL 2; SEMICOLON; CLOSE_CURLY];
]

let parser_file_tests = [
  "lexes and parses valid stage 1 C files" >::
    assert_can_parse_files_in_folder "stage_1/valid";
  "lexes but does not parse invalid stage 1 C files" >::
    assert_fails_to_parse_files_in_folder "stage_1/invalid";
  "lexes and parses valid stage 2 C files" >::
    assert_can_parse_files_in_folder "stage_2/valid";
  "lexes but does not parse invalid stage 2 C files" >::
    assert_fails_to_parse_files_in_folder "stage_2/invalid";
  "lexes and parses valid stage 3 C files" >::
    assert_can_parse_files_in_folder "stage_3/valid";
  "lexes but does not parse invalid stage 3 C files" >::
    assert_fails_to_parse_files_in_folder "stage_3/invalid";
  "lexes and parses valid stage 4 C files" >::
    assert_can_parse_files_in_folder "stage_4/valid";
  "lexes but does not parse invalid stage 4 C files" >::
    assert_fails_to_parse_files_in_folder "stage_4/invalid";
  "lexes and parses valid stage 5 C files" >::
    assert_can_parse_files_in_folder "stage_5/valid";
  "lexes and parses valid stage 5 C files in the invalid folder" >::
    assert_can_parse_files_in_folder "stage_5/invalid" ~filter:(String.is_prefix ~prefix:"syntax_err" |> Fn.non);
  "lexes and parses invalid stage 5 C files" >::
    assert_fails_to_parse_files_in_folder "stage_5/invalid" ~filter:(String.is_prefix ~prefix:"syntax_err");
]

let parser_tests = List.concat [general_parser_tests; parser_file_tests; statement_tests]