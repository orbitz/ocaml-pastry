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

let high_half_equals c1 c2 =
  (* WILL BREAK IF WE ALLOW b OTHER THAN 4 *)
  let i1 = (Char.to_int c1) lsr 4 in
  let i2 = (Char.to_int c2) lsr 4 in
  i1 = i2

let prefix_len s1 s2 =
  let rec prefix_len' = function
    | n when n < num_char -> begin
      if s1.[n] = s2.[n] then
	prefix_len' (n + 1)
      else if high_half_equals s1.[n] s2.[n] then
	n * 2 + 1
      else
	n * 2
    end
    | n ->
      n * 2
  in
  prefix_len' 0

let prefix ~b t1 t2 =
  assert (b = 4);
  prefix_len t1 t2

let digit ~b pos t =
  assert (b = 4);
  assert (pos >= 0 && pos <= (total_bits / b));
  let pos_str = pos / 2 in
  let two_digits = Char.to_int t.[pos_str] in
  (* 0xF needs to be changed if b becomes more configurable *)
  if (pos mod 2) = 0 then
    (two_digits lsr b) land 0xF
  else
    two_digits land 0xF

let closest k (t1, t2) =
  (* Guarantees lowest is returned if k is equidistant from t1, t2 *)
  let (t1, t2) =
    if compare t1 t2 < 0 then
      (t1, t2)
    else
      (t2, t1)
  in
  (*
   * The semantics of this function is finding which of
   * t1 or t2 satisfy min(abs(k - t1), abs(k - t2))
   *)
  let b = 4 in
  let prefix_t1 = prefix ~b k t1 in
  let prefix_t2 = prefix ~b k t2 in
  match prefix_t1 - prefix_t2 with
    | 0 when compare t1 t2 = 0 ->
      (* The two inputs are the same, return either *)
      t1
    | 0 -> begin
      let digit_k  = digit ~b prefix_t1 k in
      let digit_t1 = digit ~b prefix_t1 t1 in
      let digit_t2 = digit ~b prefix_t1 t2 in
      let abs_k_t1 = abs (digit_k - digit_t1) in
      let abs_k_t2 = abs (digit_k - digit_t2) in

      match abs_k_t1 - abs_k_t2 with
	| c when c < 0 ->
	  t1
	| c when c > 0 ->
	  t2
	| 0 when String.compare k t1 < 0 ->
	  t1
	| 0 ->
	  t2
    end
    | n when n < 0 ->
      (*
       * If k has fewer digits in common t1 than t2, then it means t2 must
       * be closer to k
       *)
      t2
    | _ ->
      t1
