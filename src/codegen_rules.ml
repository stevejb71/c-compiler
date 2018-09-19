open Ast
open Base

let rec leaf_statements_contain_return = function
| Return _ -> true
| Conditional (_, t, f) -> 
    leaf_statements_contain_return t || Option.value_map f ~default:false ~f:leaf_statements_contain_return
| Exp _ -> false

let leaf_block_item_contain_return = function
| Declare _ -> false
| Statement stmt -> leaf_statements_contain_return stmt

(* Not fully implemented! *)
let generate_return fundef =
  let {name; body} = fundef in
  if String.(name <> "main")
  then fundef
  else 
    if List.exists body ~f:leaf_block_item_contain_return
    then fundef
    else 
      {name; body = body @ [Statement (Return (Const 0))]}
