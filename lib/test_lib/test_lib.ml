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
