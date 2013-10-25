open Core.Std

type t = string

(*
 * This probably needs to be paramaterized
 *)
let bits_per_char = 8

let masks = [| 255 land (lnot 255) (* 00000000 *)
	    ;  255 land (lnot 127) (* 10000000 *)
	    ;  255 land (lnot 63)  (* 11000000 *)
	    ;  255 land (lnot 31)  (* 11100000 *)
	    ;  255 land (lnot 15)  (* 11110000 *)
	    ;  255 land (lnot 7)   (* 11111000 *)
	    ;  255 land (lnot 3)   (* 11111100 *)
	    ;  255 land (lnot 1)   (* 11111110 *)
            ;  255 land (lnot 0)   (* 11111111 *)
	    |]

let compare = String.compare
let of_hexstring s = failwith "nyi"
let to_hexstring t = t

let count_prefix_bits c1 c2 =
  let rec count_prefix_bits' = function
    | n when n < Array.length masks ->
      let mask = masks.(n) in
      if (c1 land mask) = (c2 land mask) then
	count_prefix_bits' (n + 1)
      else
	n - 1
    | n ->
      n
  in
  (*
   * Every byte always has 0 bits in common, so
   * start at 1
   *)
  count_prefix_bits' 1

let rec count_bits n t1 t2 =
  let c1 = t1.[n] in
  let c2 = t2.[n] in
  if c1 = c2 then
    count_bits (n + 1) t1 t2
  else
    (n * bits_per_char +
       count_prefix_bits (Char.to_int c1) (Char.to_int c2))

let common_prefix t1 t2 =
  count_bits 0 t1 t2
