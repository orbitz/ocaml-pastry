type t =
  | Node_join   of Node.t
  | Node_joined of Node.t
  | Node_state  of (Routing_table.t * Neighbourhood_set.t * Leaf_set.t)
  | Node_part   of Node.t
  | Message     of Message.t
