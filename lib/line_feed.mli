type t = Dos | Unix

val figure_out_line_ending : string -> t
val remove_crs : string -> string
val add_crs : string -> string
