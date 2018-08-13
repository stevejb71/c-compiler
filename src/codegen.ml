open Base
open Asm
open Ast

type emitter = (Asm.t -> unit)

let rec codegen_exp emitter asm = 
  let emit_binary_op e1 e2 =
    codegen_exp emitter e1;
    emitter (Push Rax);
    codegen_exp emitter e2;
    emitter (Pop Rcx)
  in match asm with
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
  | Addition (e1, e2) -> 
      emit_binary_op e1 e2;
      emitter (Addl (Ecx, Eax))
  | Subtraction (e1, e2) -> 
      emit_binary_op e2 e1;
      emitter (Subl (Ecx, Eax))
  | Multiplication (e1, e2) -> 
      emit_binary_op e1 e2;
      emitter (IMul (Ecx, Eax))
  | Division (e1, e2) -> 
      emitter (Movl {x=0; r=Eax});
      emit_binary_op e2 e1;
      emitter (IDivl (Ecx, Eax))

let codegen_stmt emitter = function
| Return e -> 
    codegen_exp emitter e;
    emitter Ret

let codegen_fundef emitter {name; body} =
  emitter (Globl name);
  emitter (Label name);
  codegen_stmt emitter body

let codegen e p = Ok (codegen_fundef e p)