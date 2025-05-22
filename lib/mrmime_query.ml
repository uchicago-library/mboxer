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

module HeaderHelpers = struct
  let poly_to_string encoder poly =
    let str = Prettym.to_string encoder poly in
    Some (Stdlib.String.trim str)

  let lookup_mrmime_field_name name =
    let lookup = [ ("date", Mrmime.Field_name.date) ] in
    Stdlib.List.assoc_opt name lookup

  let name_to_field_list mrmime_field_name parsetree =
    let open Mrmime.Header in
    assoc mrmime_field_name (header parsetree)

  (* let unpack_field_list name = function *)
  (*   | f :: _ -> field_to_string name f *)
  (*   | _ -> None *)

  (* let field_to_string name field = *)
  (*   let open Mrmime.Field in *)
  (*   match name, field with *)
  (*   | "date", Field (_, Date, poly) -> *)
  (*      poly_to_string Mrmime.Date.Encoder.date poly *)
  (*   | _ -> assert false *)

  let lookup_field_list name parsetree =
    let open Etude.Option in
    let+ encoder = lookup_mrmime_field_name name in
    name_to_field_list encoder parsetree
  (* assert false *)
end

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
  |> assoc (Mrmime.Field_name.v "subject")
  |> function
  | Field (_, Unstructured, poly) :: _ ->
    let str = Prettym.to_string unstructured poly in
    Some (Stdlib.String.trim str)
  | _ -> None

(* let subject parsetree = *)
(*   let to_ parsetree = *)
(*   let open Mrmime.Header in *)
(*   let open Mrmime.Field in *)
(*   let open Mrmime.Address.Encoder in *)
