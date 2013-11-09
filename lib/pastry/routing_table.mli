type 'a t

val create : me:Key.t -> b:int -> 'a t
val add    : node:'a Node.t -> 'a t -> 'a t
val remove : k:Key.t -> 'a t -> 'a t
val lookup : k:Key.t -> 'a t -> 'a Node.t option
val nodes  : 'a t -> 'a Node.t list
