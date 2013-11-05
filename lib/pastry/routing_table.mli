type t

val create : me:Key.t -> b:int -> t
val add    : node:Node.t -> t -> t
val remove : k:Key.t -> t -> t
val lookup : k:Key.t -> t -> Node.t option
