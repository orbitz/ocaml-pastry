type t = { key  : Key.t
	 ; body : string
	 }

val create   : dst:Node.t -> key:Key.t -> string -> t

val get_key  : t -> Key.t

val get_body : t -> string
val set_body : string -> t -> t
