open Base
open Asm
open Functions

let reg8_to_str = function
| Al -> "%al"
| Cl -> "%cl"

let reg32_to_str = function
| Eax -> "%eax"
| Ecx -> "%ecx"
| Edx -> "%edx"

let reg64_to_str = function
| Rax -> "%rax"
| Rcx -> "%rcx"

let op_reg_to_string reg_to_str operand r  =
  Printf.sprintf "%s %s" operand (reg_to_str r)

let binary_op_to_string reg_to_str operand (l, r) =
  Printf.sprintf "%s %s,%s" operand (reg_to_str l) (reg_to_str r)

let operand32_to_str = function
| I n -> Printf.sprintf "$%d" n
| R r -> reg32_to_str r
  
let asm_to_string = function
| Globl s -> ".globl " ^ s
| Label s -> s ^ ":"
| Movl (n, r) -> Printf.sprintf "movl $%d, %s" n (reg32_to_str r)
| Ret -> "ret"
| Neg r -> op_reg_to_string reg32_to_str "neg" r
| Not r -> op_reg_to_string reg32_to_str "not" r
| Cmpl (op, r) -> Printf.sprintf "cmpl %s, %s" (operand32_to_str op) (reg32_to_str r)
| Sete r -> op_reg_to_string reg8_to_str "sete" r
| Setne r -> op_reg_to_string reg8_to_str "setne" r
| Setl r -> op_reg_to_string reg8_to_str "setl" r
| Setg r -> op_reg_to_string reg8_to_str "setg" r
| Setle r -> op_reg_to_string reg8_to_str "setle" r
| Setge r -> op_reg_to_string reg8_to_str "setge" r
| Addl rs -> binary_op_to_string reg32_to_str "addl" rs
| Subl rs -> binary_op_to_string reg32_to_str "subl" rs
| IMul rs -> binary_op_to_string reg32_to_str "imul" rs
| IDivl rs -> binary_op_to_string reg32_to_str "idivl" rs
| Push r -> op_reg_to_string reg64_to_str "push" r
| Pop r -> op_reg_to_string reg64_to_str "pop" r
| Orl rs -> binary_op_to_string reg32_to_str "orl" rs
| Andb rs -> binary_op_to_string reg8_to_str "andb" rs

let emit_asm emitter = emitter >|> asm_to_string
