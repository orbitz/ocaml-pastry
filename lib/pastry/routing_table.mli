(*
 * As the name says, a table of routes for a message.  This has log_{2^b}{2^128}
 * rows, each row having 2^b - 1 entries.  So, if b = 4 there will be 32 rows.
 * Each row represents how many prefix digits (in base 2^b) the key being routed
 * and the current node have in common.  Entries in the table can be empty.
 * For example, if we need to route the key K through the routing table we find
 * the length of the common prefix between K and our node id, l.  We then find
 * the node id in the routing table at R_{l}^{K_l}.  If a node id is present
 * then we route to it.  If that fails or there is no node there, see the
 * 'rare case' section of routing in the paper.  Nodes in the routing table
 * are always the closed, based on the distance function.
 *)
open Async.Std

type t

type distance = Node.t -> int option Deferred.t

val make   : me:Node.t -> b:int -> distance -> t
val update : Node.t -> t -> t Deferred.t
val lookup : Key.t -> t -> Node.t option
