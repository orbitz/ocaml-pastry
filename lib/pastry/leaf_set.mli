(*
 * The leaf set if the nodes that are closest to the current node by
 * numeric value.  It is an even number, suggested to contain 2^b or
 * 2 * 2^b elements.  Any time a new ndoe joins we determine if it
 * is closer than one of the ones we if we are full or add it if we
 * are not.
 *)
type 'a t

(*
 * Create a leafset, the int represents half the number of
 * nodes it will store
 *)
val create : me:'a Node.t   -> int  -> 'a t
val update : node:'a Node.t -> 'a t -> ('a Node.t option * 'a t)
val remove : k:Key.t        -> 'a t -> 'a t
val nodes  : 'a t           -> 'a Node.t list

(*
 * If the key is inside the leafset, return the closest node
 *)
val contains : k:Key.t -> 'a t -> 'a Node.t option

val equal    : 'a t -> 'a t -> bool
