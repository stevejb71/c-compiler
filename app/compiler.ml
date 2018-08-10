open Core_kernel
open Asm
open Lexer
open Parser
open Codegen

let () =
  let open Result.Monad_infix in
  let filename = Sys.argv.(1) in
  let code = In_channel.read_all filename in
  let emitter = emit_asm print_endline in
  match lex code >>= parse >>= (codegen emitter) with
  | Ok _ -> ()
  | Error msg -> print_endline msg
