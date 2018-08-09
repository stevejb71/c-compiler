open OUnit2
open Lexer
open Base

let print_token = function
| OPEN_CURLY -> "<OPEN_CURLY>"
| OPEN_ROUND -> "<OPEN_ROUND>"
| IDENTIFIER -> "<IDENTIFIER>"
| SEMICOLON -> "<SEMICOLON>"
| INT_LITERAL -> "<INT_LITERAL>"
| _ -> "Unknown"

let assert_ok exp (got: (token list, string) Result.t) _ctxt = 
  let got = Result.ok_or_failwith got in
  assert_equal exp got ~printer:(fun xs -> List.map ~f:print_token xs |> String.concat ~sep:",")

let assert_error (exp: string) (got: ('a, string) Result.t) _ctxt = 
  let got = Option.value_exn (Result.error got) in
  assert_equal exp got ~printer:Fn.id
  
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
]