open Core.Std

type t = string

(*
 * This probably needs to be paramaterized
 *)
let bits_per_char = 8

let total_bits = 128

let num_char = total_bits / bits_per_char

let of_string s =
  if String.length s = num_char then
    Some (String.copy s)
  else
    None

let to_string = String.copy

let compare = String.compare

let prefix_len s1 s2 =
  let rec prefix_len' = function
    | n when n < num_char -> begin
      if s1.[n] = s2.[n] then
	prefix_len' (n + 1)
      else
	n
    end
    | n ->
      n
  in
  prefix_len' 0

let prefix ~b t1 t2 =
  assert (b = 4);
  (*
   * Since enforcing b = 4 (16 bits) find the number of common bits and
   * divide by 2, since a char is assumed to be 8 bits
   *)
  (prefix_len t1 t2) / 2

let digit ~b pos t =
  assert (b = 4);
  assert (pos >= 0 && pos <= (total_bits / (1 lsl b)));
  let pos_str = pos * 2 in
  (Char.to_int t.[pos_str]) lsl 16 + (Char.to_int t.[pos_str + 1])

