open Base

let index = ref 0

let generate_label base = 
  Int.incr index;
  Printf.sprintf "%s_%d" base !index