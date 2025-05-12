val run_cli :
  make_exe:(string list -> unit) ->
  get_exe:(string -> string -> unit) ->
  unit
