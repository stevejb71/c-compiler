open Core_kernel
open Asm_writer
open Lexer
open Parser
open Codegen
open Functions

let compile_code filename code =
  let open Result.Monad_infix in
  let emitter = print_endline >|> asm_to_string in
  match lex code >>= parse >>= (codegen emitter) with
  | Ok _ -> ()
  | Error msg -> print_endline (Printf.sprintf "Filename: %s, msg:%s" filename msg)

let compile_file filename =  compile_code filename (In_channel.read_all filename)

let () = compile_file Sys.argv.(1)