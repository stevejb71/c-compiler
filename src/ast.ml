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
| Declare of {name: string; initial_value: exp option}
| Exp of exp

type fundef = {
  name: string;
  body: stmt list;
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

let show_program ({name; body}: program): string = 
  let show_stmt = function
  | Return exp -> Printf.sprintf "return %s" (show_exp exp)
  | Declare {name; initial_value} -> 
      let init_string = Option.value_map initial_value ~default:"" ~f:(fun e -> " = " ^ show_exp e) in
      Printf.sprintf "int %s%s" name init_string
  | _ -> failwith "laters"
  in
  Printf.sprintf "%s {%s}" name (List.map ~f:show_stmt body |> String.concat ~sep:"\n")