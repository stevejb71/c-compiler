(** Performs f for each file in the folder, f receives the filename (for errors) and its contents.
    Folder name is relative to the tests folder.
 *)
val for_each_file_in_folder : foldername:string -> f:(string -> string -> unit) -> unit