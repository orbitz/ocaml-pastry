type 'a t

val create        : me:'a Node.t   -> b:int -> 'a t
val update        : node:'a Node.t -> 'a t -> 'a t
val remove        : k:Key.t        -> 'a t -> 'a t
val me            : 'a t           -> 'a Node.t

val route         : k:Key.t        -> 'a t -> 'a Node.t
val leaf_set      : 'a t           -> 'a Leaf_set.t
val routing_table : 'a t           -> 'a Routing_table.t
val nodes         : 'a t           -> 'a Node.t list
