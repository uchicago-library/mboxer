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

module Subcommands = struct
  module Make = struct
    module Arguments = struct
      let mailbox_paths =
        let open Cmdliner.Arg in
        let doc =
          "Email filepaths, to be converted into an MBOX."
        in
        let docv = "FILEPATHS" in
        let inf = info [] ~doc ~docv in
        let arg_type = pos_all file [] in
        non_empty (arg_type inf)
    end

    let make actual_exe =
      (* $ mboxer make rat.eml giraffe.eml armadillo.eml *)
      let command_term =
        let open Term in
        let+ args = Arguments.mailbox_paths in
        actual_exe args
      in
      let manpage_info =
        let description =
          "$(tname) creates an MBOX out of the emails \
           provided on the command line"
        in
        let man =
          Cmdliner.
            [ `S Manpage.s_description; `P description ]
        in
        let doc = "Creates a fresh MBOX." in
        Cmdliner.Cmd.info "make" ~doc ~man
      in
      Cmdliner.Cmd.v manpage_info command_term
  end

  module Get = struct
    (* $ mboxer get --message-id="XXX" rat.mbox *)
    module Arguments = struct
      let message_id =
        let open Cmdliner.Arg in
        let doc = "Message-ID to be searched for." in
        let docv = "MESSAGE-ID" in
        let inf = info [ "message-id" ] ~doc ~docv in
        let arg_type = opt (some string) None in
        required (arg_type inf)

      (* TODO: have this read from stdin when the user fails
         to include a path *)
      let mailbox_path =
        let open Cmdliner.Arg in
        let doc = "Filepath to the MBOX to be queried." in
        let docv = "FILEPATH" in
        let inf = info [] ~doc ~docv in
        let arg_type = pos ~rev:true 0 (some file) None in
        required (arg_type inf)
    end

    let make actual_exe =
      let command_term =
        let open Term in
        let+ id = Arguments.message_id
        and+ input_path = Arguments.mailbox_path in
        actual_exe id input_path
      in
      let manpage_info =
        let description =
          "$(tname) looks up an individual email in an \
           MBOX by message ID."
        in
        let man =
          Cmdliner.
            [ `S Manpage.s_description; `P description ]
        in
        let doc = "Gets an email by message ID." in
        Cmdliner.Cmd.info "get" ~doc ~man
      in
      Cmdliner.Cmd.v manpage_info command_term
  end

  module Summary = struct
    module Arguments = struct
      (* TODO: have this read from stdin when the user fails
         to include a path. *)
      let mailbox_path =
        let open Cmdliner.Arg in
        let doc =
          "Filepath to the MBOX to be summarized."
        in
        let docv = "FILEPATH" in
        let inf = info [] ~doc ~docv in
        let arg_type = pos ~rev:true 0 (some file) None in
        required (arg_type inf)
    end

    let make actual_exe =
      let command_term =
        let open Term in
        let+ input_path = Arguments.mailbox_path in
        actual_exe input_path
      in
      let manpage_info =
        let description =
          "$(tname) prints a summary of the contents of an \
           MBOX."
        in
        let man =
          Cmdliner.
            [ `S Manpage.s_description; `P description ]
        in
        let doc = "Summarizes an MBOX." in
        Cmdliner.Cmd.info "summary" ~doc ~man
      in
      Cmdliner.Cmd.v manpage_info command_term
  end

  let subcommands ~make_exe ~get_exe ~summary_exe =
    [ Make.make make_exe;
      Get.make get_exe;
      Summary.make summary_exe
    ]
end

let run_cli ~make_exe ~get_exe ~summary_exe =
  let open Cmdliner.Cmd in
  let doc =
    "$(tname) is a utility for manipulating MBOXes."
  in
  let inf = info "mboxer" ~doc in
  group inf
    (Subcommands.subcommands ~make_exe ~get_exe ~summary_exe)
  |> eval
  |> exit
