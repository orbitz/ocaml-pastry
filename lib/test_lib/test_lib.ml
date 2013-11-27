open Core.Std

let key_hexstring_gen =
  QCheck.Arbitrary.(lift
		      Pastry.Hexstring.encode
		      (string_len (return 16)))

let key_of_hexstring h =
  Option.value_exn
    (Pastry.Key.of_string
       (Option.value_exn
	  (Pastry.Hexstring.decode h)))

let hexstring_of_key =
  Fn.compose
    Pastry.Hexstring.encode
    Pastry.Key.to_string

let hexstring_prefix h1 h2 =
  let rec prefix = function
    | n when n < String.length h1 && n < String.length h2 && h1.[n] = h2.[n] ->
      prefix (n + 1)
    | n ->
      n
  in
  prefix 0
