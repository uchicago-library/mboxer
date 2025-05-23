module type PARSETREE = sig
  type t
  type header

  (* TODO: replace this with a real error type *)
  type error_stub

  val header : t -> header
  val backend : Backend.t

  val of_string_line_feed :
    string -> (t * Line_feed.t, error_stub) result

  val filename : t -> string option

  (* argument for parsing: we avoid overcounting MIME
     multipart parts that look like this: *)

  (* content-disposition: attachment *)
  (* content-type: text/plain *)

  (* content-disposition: attachment *)

  (* or if you had an email with *)

  (* content-disposition: attachment *)

  (* in the body and it didn't even use MIME, that would
     increment the count *)
end

module type QUERY = sig
  include PARSETREE

  val date : t -> string option
  val from : t -> string option
  val to_ : t -> string option
  val subject : t -> string option
  val message_id : t -> string option
  val cc : t -> string option
  val content_disposition : t -> string option
  (* val attachment_count : t -> int *)
end
