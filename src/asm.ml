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

type operand32 =
| I of int
| R of register32

type t =
| Globl of string
| Label of string
| Movl of (int * register32)
| Ret
| Neg of register32
| Not of register32
| Cmpl of (operand32 * register32)
| Sete of register8
| Setne of register8
| Setl of register8
| Setg of register8
| Setle of register8
| Setge of register8
| Push of register64
| Pop of register64
| Addl of (register32 * register32)
| Subl of (register32 * register32)
| IMul of (register32 * register32)
| IDivl of (register32 * register32)
| Andb of (register8 * register8)
| Orl of (register32 * register32)