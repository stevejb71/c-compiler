open Base

type register =
| Eax

type t =
| Globl of string
| Label of string
| Movl of {x: int; r: register}
| Ret

let reg_to_str = function
| Eax -> "%eax"

let emit_asm (emitter: string -> unit) = function
| Globl s -> emitter (".globl " ^ s)
| Label s -> emitter s
| Movl {x; r} -> emitter (Printf.sprintf "movl $%d %s" x (reg_to_str r))
| Ret -> emitter "ret"