open Base

type exp =
| Const of int
| Complement of exp
| Negation of exp
| Logical_Negation of exp
| Addition of exp * exp
| Subtraction of exp * exp
| Multiplication of exp * exp
| Division of exp * exp
| Logical_And of exp * exp
| Logical_Or of exp * exp
| Equality of exp * exp
| Inequality of exp * exp
| LessThan of exp * exp
| LessThanOrEqual of exp * exp
| GreaterThan of exp * exp
| GreaterThanOrEqual of exp * exp

type stmt =
| Return of exp

type fundef = {
  name: string;
  body: stmt;
}

type program = fundef


let rec show_exp e = 
  let show_binary_exp op e1 e2 =
    Printf.sprintf "%s %s %s" op (show_exp e1) (show_exp e2)
  in
  match e with
  | Const n -> Int.to_string n
  | Complement e -> "~" ^ show_exp e
  | Negation e -> "-" ^ show_exp e
  | Logical_Negation e -> "!" ^ show_exp e
  | Addition (e1, e2) -> show_binary_exp "+" e1 e2
  | Subtraction (e1, e2) -> show_binary_exp "-" e1 e2
  | Multiplication (e1, e2) -> show_binary_exp "*" e1 e2
  | Division (e1, e2) -> show_binary_exp "/" e1 e2
  | Logical_And (e1, e2) -> show_binary_exp "&&" e1 e2
  | Logical_Or (e1, e2) -> show_binary_exp "||" e1 e2
  | Equality (e1, e2) -> show_binary_exp "==" e1 e2
  | Inequality (e1, e2) -> show_binary_exp "!=" e1 e2
  | LessThan (e1, e2) -> show_binary_exp "<" e1 e2
  | LessThanOrEqual (e1, e2) -> show_binary_exp "<=" e1 e2
  | GreaterThan (e1, e2) -> show_binary_exp ">" e1 e2
  | GreaterThanOrEqual (e1, e2) -> show_binary_exp ">=" e1 e2

let show_program ({name; body}: program): string = 
  let show_stmt = function
  | Return exp -> Printf.sprintf "return %s" (show_exp exp)
  in
  Printf.sprintf "%s {%s}" name (show_stmt body)