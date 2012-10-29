type t =
  | Node_join of Node.t
  | Node_part of Node.t
  | Message   of Message.t
