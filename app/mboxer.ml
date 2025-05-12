let make_exe = List.iter print_endline

let get_exe id input_path =
  let open Printf in
  printf "message id: %s\n" id ;
  printf "input filepath: %s\n" input_path

let () = Mboxer.Cli.run_cli ~make_exe ~get_exe
