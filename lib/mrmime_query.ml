type header = Mrmime.Header.t
type t = Mrmime.Header.t * string Mrmime.Mail.t

let backend = Backend.Mrmime

(* TODO: replace this with the real error type *)
type error_stub = string

let of_string email_string =
  email_string
  |> Angstrom.parse_string ~consume:All
       (Mrmime.Mail.mail None)

let of_string_line_feed email_str =
  let open Etude.Result.Make (struct
    type t = error_stub
  end) in
  let lf_type =
    Line_feed.figure_out_line_ending email_str
  in
  let processed =
    match lf_type with
    | Unix -> Line_feed.add_crs email_str
    | Dos -> email_str
  in
  let* parsed = of_string processed in
  Ok (parsed, lf_type)

let header = fst

module HeaderGet = struct
  let date parsetree =
    let open Mrmime.Header in
    let open Mrmime.Field in
    let open Mrmime.Date.Encoder in
    parsetree
    |> header
    |> assoc Mrmime.Field_name.date
    |> function
    | Field (_, Date, poly) :: _ ->
      let str = Prettym.to_string date poly in
      Some (Stdlib.String.trim str)
    | _ -> None

  let from parsetree =
    let open Mrmime.Header in
    let open Mrmime.Field in
    let open Mrmime.Mailbox.Encoder in
    parsetree
    |> header
    |> assoc Mrmime.Field_name.from
    |> function
    | Field (_, Mailboxes, poly) :: _ ->
      let str = Prettym.to_string mailboxes poly in
      Some (Stdlib.String.trim str)
    | _ -> None

  let to_ parsetree =
    let open Mrmime.Header in
    let open Mrmime.Field in
    let open Mrmime.Address.Encoder in
    parsetree
    |> header
    |> assoc (Mrmime.Field_name.v "to")
    |> function
    | Field (_, Addresses, poly) :: _ ->
      let str = Prettym.to_string addresses poly in
      Some (Stdlib.String.trim str)
    | _ -> None

  let subject parsetree =
    let open Mrmime.Header in
    let open Mrmime.Field in
    let open Mrmime.Unstructured.Encoder in
    parsetree
    |> header
    |> assoc Mrmime.Field_name.subject
    |> function
    | Field (_, Unstructured, poly) :: _ ->
      let str = Prettym.to_string unstructured poly in
      Some (Stdlib.String.trim str)
    | _ -> None

  let message_id parsetree =
    let open Mrmime.Header in
    let open Mrmime.Field in
    let open Mrmime.MessageID.Encoder in
    parsetree
    |> header
    |> assoc Mrmime.Field_name.message_id
    |> function
    | Field (_, MessageID, poly) :: _ ->
      let str = Prettym.to_string message_id poly in
      Some (Stdlib.String.trim str)
    | _ -> None

  let cc parsetree =
    let open Mrmime.Header in
    let open Mrmime.Field in
    let open Mrmime.Address.Encoder in
    parsetree
    |> header
    |> assoc Mrmime.Field_name.cc
    |> function
    | Field (_, Addresses, poly) :: _ ->
      let str = Prettym.to_string addresses poly in
      Some (Stdlib.String.trim str)
    | _ -> None

  (* let content_disposition parsetree = TODO *)
end

include HeaderGet

module Attachments = struct
  (* copied out of attc *)
  (* TODO: fix these *)

  (* let is_attachment = *)
  (*   header *)
  (*   >> content_disposition *)
  (*   >> Option.map *)
  (*        ( Header.Field.Value.value *)
  (*          >> String.lowercase_ascii *)
  (*          >> fun x -> x = "attachment" || x = "inline" ) *)
  (*   >> Option.default false *)

  (* let to_attachment tree = *)
  (*   let open Mrmime.Mail in *)
  (*   if is_attachment tree *)
  (*   then *)
  (*     match tree with *)
  (*     | header, Leaf data -> *)
  (*        Some (Attachment.make header data) *)
  (*     | _ -> None *)
  (*   else None *)

  (* let rec attachments tree = *)
  (*   let open Mrmime.Mail in *)
  (*   match tree with *)
  (*   | header, Leaf data -> *)
  (*      Option.to_list (to_attachment (header, Leaf data)) *)
  (*   | _, Multipart parts -> *)
  (*      let with_data ats (h, d) = *)
  (*        match d with *)
  (*        | None -> ats *)
  (*        | Some d -> (h, d) :: ats *)
  (*      in *)
  (*      List.flatmap attachments *)
  (*        (List.foldl with_data [] parts) *)
  (*   | _, Message (h, b) -> attachments (h, b) *)
end
