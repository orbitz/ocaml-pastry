(*
 * The leaf set if the nodes that are closest to the current node by
 * numeric value.  It is an even number, suggested to contain 2^b or
 * 2 * 2^b elements.  Any time a new ndoe joins we determine if it
 * is closer than one of the ones we if we are full or add it if we
 * are not.
 *)
type t

val make   : me:Node.t -> int -> t
val update : Node.t -> t
val lookup : Key.t -> t -> Node.t option
