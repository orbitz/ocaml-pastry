open Core.Std

type t = string

let is_hex = function
  | 'a'..'f' | 'A'..'F' -> true
  | _                   -> false

let validate_char c =
  Char.is_digit c && is_hex c

let rec validate_str ~pos s =
  match pos with
    | pos when pos < String.length s && validate_char s.[pos] ->
      validate_str ~pos:(pos + 1) s
    | pos when pos < String.length s ->
      None
    | _ ->
      Some s

let of_string s = validate_str ~pos:0
let to_string t = t
let to_bytestring t =
  let s = String.create (String.length t / 2 + String.length t mod 2) in
  for i = 0 to String.length s do
    
  done;
let compare     = String.compare

