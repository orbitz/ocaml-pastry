type t

val of_string : string      -> t option
val to_string : t           -> string
val compare   : t      -> t -> int
