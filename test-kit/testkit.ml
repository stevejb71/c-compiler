open Core_kernel

let foldernames = [
  "tests/stage_1/valid";
  "tests/stage_2/valid";
  "tests/stage_3/valid";
  "tests/stage_4/valid";
]

type outputs = {
  gcc_output: string;
  mycc_output: string;
}

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  String.strip r

let run_script_on_file script filename =
  run @@ script ^ " " ^ filename
  
let compare_compilation filename: outputs option =
  let gcc_output = run_script_on_file "./test-kit/run-gcc.sh " filename in
  let mycc_output = run_script_on_file "./test-kit/run-mycc.sh " filename in
  if gcc_output = mycc_output
  then None
  else Some {gcc_output; mycc_output}

let c_files_in_folder foldername =
  let all_files = Sys.readdir foldername in
  Array.filter all_files ~f:(fun n -> String.is_suffix n ~suffix:".c") 
  |> Array.to_list
  |> List.map ~f:(fun f -> foldername ^ "/" ^ f)

let () =
  let open List.Monad_infix in
  let failures = foldernames >>= fun foldername ->
  c_files_in_folder foldername >>= fun c_file ->
  [(c_file, compare_compilation c_file)] |>
  List.filter ~f:(fun (_, res) -> Option.is_some res) |>
  List.map ~f:(fun (filename, res) -> (filename, Option.value_exn res)) in
  if not @@ List.is_empty failures 
  then begin
    Stdio.print_endline (Printf.sprintf "%d files gcc/mycc outputs didn't match" (List.length failures));
    List.iter failures ~f:(fun (filename, {gcc_output; mycc_output}) ->
      Stdio.print_endline (Printf.sprintf "File:%s | gcc:'%s' | mycc: '%s'" filename gcc_output mycc_output)
    )
  end
