open Core.Std

val key_hexstring_gen : string QCheck.Arbitrary.t

val key_of_hexstring : string -> Pastry.Key.t

val hexstring_of_key : Pastry.Key.t -> string
