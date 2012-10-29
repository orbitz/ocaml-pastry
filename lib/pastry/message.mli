type t = { dst  : Node.t
	 ; key  : Key.t
	 ; body : string
	 }

val create   : dst:Node.t -> key:Key.t -> string -> t

val get_dst  : t -> Node.t
val set_dst  : Node.t -> t -> t

val get_key  : t -> Key.t

val get_body : t -> string
val set_bdoy : string -> t -> t
