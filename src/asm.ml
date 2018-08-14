open Base

type register8 =
| Al
| Cl

type register32 =
| Eax
| Ecx
| Edx

type register64 =
| Rax
| Rcx

type t =
| Globl of string
| Label of string
| Movl of (int * register32)
| Ret
| Neg of register32
| Not of register32
| Cmpl of (int * register32)
| Sete of register8
| Setne of register8
| Push of register64
| Pop of register64
| Addl of (register32 * register32)
| Subl of (register32 * register32)
| IMul of (register32 * register32)
| IDivl of (register32 * register32)
| Andb of (register8 * register8)
| Orl of (register32 * register32)

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
  
let asm_to_string = function
| Globl s -> ".globl " ^ s
| Label s -> s ^ ":"
| Movl (x, r) -> Printf.sprintf "movl $%d, %s" x (reg32_to_str r)
| Ret -> "ret"
| Neg r -> op_reg_to_string reg32_to_str "neg" r
| Not r -> op_reg_to_string reg32_to_str "not" r
| Cmpl (x, r) -> Printf.sprintf "cmpl $%d, %s" x (reg32_to_str r)
| Sete r -> op_reg_to_string reg8_to_str "sete" r
| Setne r -> op_reg_to_string reg8_to_str "setne" r
| Addl rs -> binary_op_to_string reg32_to_str "addl" rs
| Subl rs -> binary_op_to_string reg32_to_str "subl" rs
| IMul rs -> binary_op_to_string reg32_to_str "imul" rs
| IDivl rs -> binary_op_to_string reg32_to_str "idivl" rs
| Push r -> op_reg_to_string reg64_to_str "push" r
| Pop r -> op_reg_to_string reg64_to_str "pop" r
| Orl rs -> binary_op_to_string reg32_to_str "orl" rs
| Andb rs -> binary_op_to_string reg8_to_str "andb" rs

let emit_asm (emitter: string -> unit) asm = emitter (asm_to_string asm)
