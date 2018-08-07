open OUnit2
open Lexer
open Base

let print_token = function
| OPEN_CURLY -> "<OPEN_CURLY>"
| _ -> "Unknown"

let ae exp got _test_ctxt = assert_equal exp got ~printer:(fun xs -> List.map ~f:print_token xs |> String.concat ~sep:",")

let lexer_tests = [
  "empty string has no tokens" >::
    ae [] (lex "");
  "single open curly bracket produces a OPEN_CURLY token" >::
    ae [OPEN_CURLY] (lex "{")
]