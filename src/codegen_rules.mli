open Ast

(* For the main functon, generate a return at the end even if none has been written *)
val generate_return: Ast.fundef -> Ast.fundef