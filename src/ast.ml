open Base

type exp =
| Const of int
| Complement of exp
| Negation of exp
| Logical_Negation of exp

type stmt =
| Return of exp

type fundef = {
  name: string;
  body: stmt;
}

type program = fundef

let show_program ({name; body}: program): string = 
  let rec show_exp = function
  | Const n -> Int.to_string n
  | Complement e -> "~" ^ show_exp e
  | Negation e -> "-" ^ show_exp e
  | Logical_Negation e -> "!" ^ show_exp e
  in
  let show_stmt = function
  | Return exp -> Printf.sprintf "return %s" (show_exp exp)
  in
  Printf.sprintf "%s {%s}" name (show_stmt body)