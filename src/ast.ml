open Base

type exp =
| Const of int

type stmt =
| Return of exp

type fundef = {
  name: string;
  body: stmt;
}

type program = fundef

let show_program ({name; body}: program): string = 
  let show_exp = function
  | Const n -> Int.to_string n
  in
  let show_stmt = function
  | Return exp -> Printf.sprintf "return %s" (show_exp exp)
  in
  Printf.sprintf "%s {%s}" name (show_stmt body)