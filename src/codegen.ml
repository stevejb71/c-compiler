open Base
open Asm
open Ast

type emitter = (Asm.t -> unit)

let rec codegen_exp emitter = function
| Const x -> 
    emitter (Movl {x; r=Eax})
| Complement e -> 
    codegen_exp emitter e;
    emitter (Not Eax)
| Negation e -> 
    codegen_exp emitter e;
    emitter (Neg Eax)
| Logical_Negation e -> 
    codegen_exp emitter e;
    emitter (Cmpl {x=0; r=Eax});
    emitter (Movl {x=0; r=Eax});
    emitter (Sete Al)

let codegen_stmt emitter = function
| Return e -> 
    codegen_exp emitter e;
    emitter Ret

let codegen_fundef emitter {name; body} =
  emitter (Globl name);
  emitter (Label name);
  codegen_stmt emitter body

let codegen e p = Ok (codegen_fundef e p)