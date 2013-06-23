(*
 * A Key is a 128 bit identifier that can be converted to and
 * from a string.  Keys can also be tested for having a common
 * prefix.  The prefix takes two Key's and the number of bits
 * of the prefix is returned.
 *)
type t

val compare       : t      -> t -> int
val of_string     : string ->      t option
val to_string     : t      ->      string
val common_prefix : t      -> t -> int
