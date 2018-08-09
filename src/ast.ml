type exp =
| Const of int

type stmt =
| Return of exp

type fundef = {
  name: string;
  body: stmt;
}

type program = fundef

