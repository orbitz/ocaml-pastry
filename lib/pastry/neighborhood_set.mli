(*
 * Represents the nodes closets in proximity to ourself.  This only used
 * in routing during the unlikely event node is not found in the routing table
 *)
open Async.Std

type t

type distance = Node.t -> int option Deferred.t

val make   : me:Node.t -> int -> distance -> t
val update : Node.t -> t -> t Deferred.t
val lookup : Key.t -> t -> Node.t option
