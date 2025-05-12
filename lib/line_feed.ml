type t = Dos | Unix

let figure_out_line_ending email_string =
  let open Prelude.String in
  let not_cr c = not (contains "\r\n" c) in
  match dropwhile not_cr email_string with
  | "" -> Unix
  | nonempty -> begin
    match nonempty.[0] with
    | '\r' -> Dos
    | _ -> Unix
  end

let remove_crs str =
  let b = Buffer.create 0 in
  let mk_new_string () =
    let each_char c =
      match c with
      | '\r' -> ()
      | _ -> Buffer.add_char b c
    in
    String.iter each_char str
  in
  mk_new_string () ;
  Buffer.contents b

let add_crs str =
  let b = Buffer.create 0 in
  let mk_new_string () =
    let each_char c =
      match c with
      | '\n' ->
        Buffer.add_char b '\r' ;
        Buffer.add_char b '\n'
      | _ -> Buffer.add_char b c
    in
    String.iter each_char str
  in
  mk_new_string () ;
  Buffer.contents b
