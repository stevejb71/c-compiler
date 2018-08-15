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
  in
  let emit_comparison e1 e2 =
    codegen_exp emitter e1;
    emitter (Push Rax);
    codegen_exp emitter e2;
    emitter (Pop Rcx);
    emitter (Cmpl (R Eax, Ecx));
    emitter (Movl (0, Eax));
  in 
  match asm with
  | Const x -> 
      emitter (Movl (x, Eax))
  | Complement e -> 
      codegen_exp emitter e;
      emitter (Not Eax)
  | Negation e -> 
      codegen_exp emitter e;
      emitter (Neg Eax)
  | Logical_Negation e -> 
      codegen_exp emitter e;
      emitter (Cmpl (I 0, Eax));
      emitter (Movl (0, Eax));
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
      emitter (Movl (0, Edx));
      codegen_exp emitter e2;
      emitter (Push Rax);
      codegen_exp emitter e1;
      emitter (Pop Rcx);
      emitter (IDivl (Ecx, Eax))
  | Equality (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Sete Al);
  | Inequality (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setne Al);
  | LessThan (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setl Al);
  | LessThanOrEqual (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setle Al);
  | GreaterThan (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setg Al);
  | GreaterThanOrEqual (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setge Al)
  | Logical_And (e1, e2) ->
      codegen_exp emitter e1;
      emitter (Push Rax);
      codegen_exp emitter e2;
      emitter (Pop Rcx);
      emitter (Cmpl (I 0, Ecx));
      emitter (Setne Cl);
      emitter (Cmpl (I 0, Eax));
      emitter (Movl (0, Eax));
      emitter (Setne Al);
      emitter (Andb (Cl, Al))
  | Logical_Or (e1, e2) ->
      codegen_exp emitter e1;
      emitter (Push Rax);
      codegen_exp emitter e2;
      emitter (Pop Rcx);
      emitter (Orl (Ecx, Eax));
      emitter (Movl (0, Eax));
      emitter (Setne Al)

let codegen_stmt emitter = function
| Return e -> 
    codegen_exp emitter e;
    emitter Ret

let codegen_fundef emitter {name; body} =
  emitter (Globl name);
  emitter (Label name);
  codegen_stmt emitter body

let codegen e p = Ok (codegen_fundef e p)