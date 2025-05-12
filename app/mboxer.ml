let make_exe = function
  | [] -> print_endline "USAGE: mboxer make [FILEPATHS]"
  | paths -> List.iter print_endline paths

let () = Mboxer.Cli.run_cli ~make_exe


(* Local Variables: *)
(* mode: tuareg *)
(* End: *)
