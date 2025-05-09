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
  let dummy =
    let open Cmdliner.Arg in
    let doc =
      "Dummy CLI argument for $(tname)."
    in
    let docv = "D" in
    let inf = info [ "d"; "dummy-mode" ] ~doc ~docv in
    let arg_type = flag in
    value (arg_type inf)
end

module Subcommands = struct
  module Get = struct
    let cmnd = "dude"
  end
  module Make = struct
    let cmnd = "bro"
  end
  let subcommands = [ Get.cmnd ; Make.cmnd ]
end
