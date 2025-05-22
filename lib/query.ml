module type PARSETREE = sig
  type t
  type header

  (* TODO: replace this with a real error type *)
  type error_stub

  val header : t -> header
  val backend : Backend.t

  val of_string_line_feed :
    string -> (t * Line_feed.t, error_stub) result
end

module type QUERY = sig
  include PARSETREE

  val date : t -> string option
  val from : t -> string option
  val to_ : t -> string option
  val subject : t -> string option
  (* val message_id : t -> string option *)
  (* val cc : t -> string option *)
  (* val attachment_count : t -> int *)
end
