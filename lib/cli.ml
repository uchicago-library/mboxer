module type TERM =
  Etude.Endofunctors_intf.Applicative.AUGMENTED

module Term : TERM with type 'a t = 'a Cmdliner.Term.t =
struct
  module type BASIC =
    Etude.Endofunctors_intf.Applicative.BASIC

  module BasicApp :
    BASIC with type 'a t = 'a Cmdliner.Term.t = struct
    open Cmdliner.Term

    type 'a t = 'a Cmdliner.Term.t

    let map f x = const f $ x
    let product ax ay = map (fun x y -> (x, y)) ax $ ay
    let unit = const ()
  end

  open Etude.Endofunctors
  include Applicative.Make (BasicApp)
end

module Arguments = struct
  let mailbox_paths =
    let open Cmdliner.Arg in
    let doc =
      "Email filepaths, to be converted into an MBOX."
    in
    let docv = "FILEPATHS" in
    let inf = info [] ~doc ~docv in
    let arg_type = pos_all string [] in
    value (arg_type inf)
end

module Subcommands = struct
  module Make = struct
    (* $ mboxer make rat.eml giraffe.eml armadillo.eml *)
    let actual_exe = function
      | [] -> print_endline "USAGE: mboxer make [FILEPATHS]"
      | paths -> List.iter print_endline paths
    let term =
      let open Term in
      let+ args = Arguments.mailbox_paths
      in actual_exe args
    let manpage_info =
      let description =
        "$(tname) creates an MBOX out of the emails provided on the \
         command line"
      in
      let man =
        Cmdliner.[ `S Manpage.s_description; `P description ]
      in
      let doc = "Creates a fresh MBOX." in
      Cmdliner.Cmd.info "make" ~doc ~man
    let command = Cmdliner.Cmd.v manpage_info term
  end
  module Get = struct
    (* $ mboxer get --message-id="XXX" rat.mbox *)
  end
  let subcommands = [
      Make.command ;
      (* Get.command ; *)
    ]
end

module Executable = struct
  let exe () =
    let open Cmdliner.Cmd in
    let doc =
      "$(tname) is a utility for manipulating MBOXes."
    in
    let inf = info "mboxer" ~doc in
    group inf Subcommands.subcommands
    |> eval
    |> exit
end
