open Base
open OUnit2
open Asm_writer 

let assert_asm_strings_equal exp asm _ctxt =
  let got = asm_to_string asm in
  assert_equal exp got ~printer:Fn.id

let asm_writer_tests = [
  "movl on val + reg" >::
    assert_asm_strings_equal "movl $42, %eax" (Movl (I 42,Eax));
  "movl on reg + reg" >::
    assert_asm_strings_equal "movl %ecx, %eax" (Movl (R Ecx,Eax));
  "cmpl on val + reg" >::
    assert_asm_strings_equal "cmpl $32, %edx" (Cmpl (I 32,Edx));
  "cmpl on reg + reg" >::
    assert_asm_strings_equal "cmpl %ecx, %edx" (Cmpl (R Ecx,Edx));
]