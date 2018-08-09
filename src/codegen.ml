open Base
open Asm
open Ast

type emitter = (Asm.t -> unit)

let codegen_stmt emitter = function
| Return (Const x) -> 
    emitter (Movl {x; r=Eax});
    emitter Ret

let codegen_fundef emitter {name; body} =
  emitter (Globl name);
  emitter (Label name);
  codegen_stmt emitter body

let codegen e p = Ok (codegen_fundef e p)