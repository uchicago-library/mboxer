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

let date parsetree =
  let open Mrmime.Header in
  let open Mrmime.Field in
  let open Mrmime.Date.Encoder in
  parsetree
  |> header
  |> assoc Mrmime.Field_name.date
  |> List.hd
  |> function
  | Field (_, Date, poly) ->
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
  |> List.hd
  |> function
  | Field (_, Mailboxes, poly) ->
    let str = Prettym.to_string mailboxes poly in
    Some (Stdlib.String.trim str)
  | _ -> None

let to_ parsetree =
  let open Mrmime.Header in
  let open Mrmime.Field in
  let open Mrmime.Unstructured.Encoder in
  parsetree
  |> header
  |> assoc (Mrmime.Field_name.v "to")
  |> List.hd
  |> function
  | Field (_, Unstructured, poly) ->
    let str = Prettym.to_string unstructured poly in
    Some (Stdlib.String.trim str)
  | _ -> None
