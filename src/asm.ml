open Base

type register =
| Eax
| Rax
| Ecx
| Rcx
| Edx
| Al

type t =
| Globl of string
| Label of string
| Movl of (int * register)
| Ret
| Neg of register
| Not of register
| Cmpl of (int * register)
| Sete of register
| Push of register
| Pop of register
| Addl of (register * register)
| Subl of (register * register)
| IMul of  (register * register)
| IDivl of  (register * register)

let reg_to_str = function
| Eax -> "%eax"
| Rax -> "%rax"
| Ecx -> "%ecx"
| Rcx -> "%rcx"
| Edx -> "%edx"
| Al -> "%al"

let op_reg_to_string operand r =
  Printf.sprintf "%s %s" operand (reg_to_str r)

let binary_op_to_string operand (l, r) =
  Printf.sprintf "%s %s,%s" operand (reg_to_str l) (reg_to_str r)
  
let asm_to_string = function
| Globl s -> ".globl " ^ s
| Label s -> s ^ ":"
| Movl (x, r) -> Printf.sprintf "movl $%d, %s" x (reg_to_str r)
| Ret -> "ret"
| Neg r -> op_reg_to_string "neg" r
| Not r -> op_reg_to_string "not" r
| Cmpl (x, r) -> Printf.sprintf "cmpl $%d, %s" x (reg_to_str r)
| Sete r -> op_reg_to_string "sete" r
| Addl rs -> binary_op_to_string "addl" rs
| Subl rs -> binary_op_to_string "subl" rs
| IMul rs -> binary_op_to_string "imul" rs
| IDivl rs -> binary_op_to_string "idivl" rs
| Push r -> op_reg_to_string "push" r
| Pop r -> op_reg_to_string "pop" r

let emit_asm (emitter: string -> unit) asm = emitter (asm_to_string asm)
