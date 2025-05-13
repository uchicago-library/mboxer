type t = Mrmime | Ocamlnet | Email_message

let to_string = function
  | Ocamlnet -> "OCamlnet"
  | Mrmime -> "Mr. Mime"
  | Email_message -> "Email_message"
