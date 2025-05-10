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
      "Filepaths to emails, to be converted into an MBOX."
    in
    let docv = "FILEPATHS" in
    let inf = info [] ~doc ~docv in
    let arg_type = pos_all (some string) [] in
    value (arg_type inf)
end

module Subcommands = struct
  module Make = struct
    let make_term exe mailbox_paths =
      let open Term in
      let+ make = pure exe
      in make mailbox_paths
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
    let command exe mailbox_paths =
      let open Cmdliner.Cmd in
      let term = make_term exe mailbox_paths in
      v manpage_info term
  end
  let subcommands make_exe mailbox_paths = [
      Make.command make_exe mailbox_paths ;
    ]
  (* let megacommand = *)
  (*   let inf = info "mboxer" ~doc in *)
  (*   Cmdliner.Cmd.group inf (subcommands) *)
end

module Executable = struct

  (* let exe = *)
  (*   let open Cmdliner.Cmd in *)
  (*   let open Term in *)
  (*   let open Arguments in *)
  (*   let open Subcommands in *)
  (*   let doc = "$(tname) is a utility for manipulating MBOXes." in *)
  (* let inf = info "mboxer" ~doc in *)
  (*   group inf (subcommands make_term ) *)
end

(* $ mboxer get --message-id="XXX" rat.mbox *)
(* $ mboxer make rat.eml giraffe.eml armadillo.eml *)

