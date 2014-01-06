(*
 * A Key is a 128 bit identifier that can be converted to and
 * from a string.  Keys can also be tested for having a common
 * prefix.  The prefix takes two Key's and the number of bits
 * of the prefix is returned.
 *)
type t

val of_string : string -> t option
val to_string : t      -> string

val compare   : t      -> t   -> int
val equal     : t      -> t   -> bool
val prefix    : b:int  -> t   -> t -> int
val digit     : b:int  -> int -> t -> int
val closest   : t      -> (t * t)  -> t
