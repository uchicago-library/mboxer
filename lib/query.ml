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

  val date : t -> string option
  val from : t -> string option
  val message_id : t -> string option
end
