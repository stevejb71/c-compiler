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
| Assign of string * exp
| Var of string

type stmt =
| Return of exp
| Exp of exp
| Conditional of (exp * stmt * stmt option)

type block_item =
| Statement of stmt
| Declare of {name: string; initial_value: exp option}

type fundef = {
  name: string;
  body: block_item list;
}

type program = fundef

let rec show_exp e = 
  let show_binary_exp op e1 e2 =
    Printf.sprintf "%s %s %s" (show_exp e1) op (show_exp e2)
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
  | Assign (n, e) -> Printf.sprintf "%s = %s" n (show_exp e)
  | Var n -> n
and show_stmt = function
| Return e -> Printf.sprintf "Return %s" (show_exp e)
| Exp e -> show_exp e
| Conditional (c, t ,None) -> Printf.sprintf "if %s then %s" (show_exp c) (show_stmt t)
| Conditional (c, t ,Some f) -> Printf.sprintf "if %s then %s else %s" (show_exp c) (show_stmt t) (show_stmt f)

let show_block_item = function
| Declare {name; initial_value=None} -> Printf.sprintf "int %s" name
| Declare {name; initial_value=Some body} -> Printf.sprintf "int %s = %s" name (show_exp body)
| Statement stmt -> show_stmt stmt

let show_program ({name; body}: program): string = 
  Printf.sprintf "%s {%s}" name (List.map ~f:show_block_item body |> String.concat ~sep:"\n")
