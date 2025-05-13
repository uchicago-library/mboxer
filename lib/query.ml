module type PARSETREE = sig
  type t

  (* TODO: replace this with a real error type *)
  type error_stub

  val backend : Backend.t

  val of_string_line_feed :
    string -> (t * Line_feed.t, error_stub) result

  val to_string_line_feed :
    ?line_feed:Line_feed.t -> t -> string
end

module type QUERY = sig
  include PARSETREE

  val date_v : t -> string option
  val from_v : t -> string option
  val to_v : t -> string option
  val subject_v : t -> string option
  val message_id_v : t -> string option
  val attachment_count : t -> int
end
