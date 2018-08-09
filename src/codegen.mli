open Base
open Asm
open Ast

type emitter = (Asm.t -> unit)

val codegen : emitter -> program -> (unit, string) Result.t