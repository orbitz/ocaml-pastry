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

let closest k (t1, t2) =
  (*
   * The semantics of this function is finding which of
   * t1 or t2 satisfy min(abs(k - t1), abs(k - t2))
   *)
  let b = 4 in
  let prefix_t1 = prefix ~b k t1 in
  let prefix_t2 = prefix ~b k t2 in
  match prefix_t1 - prefix_t2 with
    | 0 when compare t1 t2 = 0 ->
      (* They are the all same *)
      k
    | 0 ->
      (*
       * They have the same number of digits in common, that means
       * they must differ on the next digit.  And digits are actual ints
       * so we know we can extract those and subtract them to find the closest
       *)
      let digit_k  = digit ~b prefix_t1 k in
      let digit_t1 = digit ~b prefix_t1 t1 in
      let digit_t2 = digit ~b prefix_t1 t2 in
      if abs (digit_k - digit_t1) < abs (digit_k - digit_t2) then
	t1
      else
	t2
    | n when n < 0 ->
      (*
       * If k has fewer digits in common t1 than t2, then it means t2 must
       * be closer to k
       *)
      t2
    | _ ->
      t1
