open Core_kernel

let for_each_file_in_folder ~foldername ~f =
  let foldername = "../../../tests/" ^ foldername in
  let c_files = Sys.readdir foldername in
  Array.iter c_files ~f:(fun filename -> f filename (In_channel.read_all (foldername ^ "/" ^ filename)))
