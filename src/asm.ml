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
| Rbp
| Rsp

type operand32 =
| I of int
| R of register32

type operand64 =
| I of int
| R of register64
| O of (int * register64) (* offset *)

type dest_operand64 =
| R of register64
| O of (int * register64) (* offset *)

type register =
| R32 of register32
| R64 of register64

type t =
| Globl of string
| Label of string
| Movl of (operand32 * register32)
| Movq of (operand64 * dest_operand64)
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