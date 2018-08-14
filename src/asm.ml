open Base

type register =
| Eax
| Al

type t =
| Globl of string
| Label of string
| Movl of {x: int; r: register}
| Ret
| Neg of register
| Not of register
| Cmpl of {x:int; r:register}
| Sete of register

let reg_to_str = function
| Eax -> "%eax"
| Al -> "%al"

let op_reg_to_string operand r =
  Printf.sprintf "%s %s" operand (reg_to_str r)

let asm_to_string = function
| Globl s -> ".globl " ^ s
| Label s -> s ^ ":"
| Movl {x; r} -> Printf.sprintf "movl $%d, %s" x (reg_to_str r)
| Ret -> "ret"
| Neg r -> op_reg_to_string "neg" r
| Not r -> op_reg_to_string "not" r
| Cmpl {x; r} -> Printf.sprintf "cmpl $%d, %s" x (reg_to_str r)
| Sete r -> op_reg_to_string "sete" r

let emit_asm (emitter: string -> unit) asm = emitter (asm_to_string asm)
