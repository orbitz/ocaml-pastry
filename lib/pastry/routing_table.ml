open Core.Std

module Routing_table = Map.Make(struct
 (*
  * (length of common prefix * digit value at that position)
  *)
  type t = (int * int) with compare, sexp
end)

type 'a t = { me    : Key.t
	    ; b     : int
	    ; table : 'a Node.t Routing_table.t
	    }

let create ~me ~b =
  { me; b; table = Routing_table.empty }

let prefix_digit ~me b key =
  let prefix = Key.prefix ~b key me in
  let digit  = Key.digit ~b prefix key in
  (prefix, digit)

let add ~node t =
  let pdiddy = prefix_digit ~me:t.me t.b (Node.key node) in
  { t with table = Routing_table.add ~key:pdiddy ~data:node t.table }

let remove ~k t =
  let pdiddy = prefix_digit ~me:t.me t.b k in
  {t with table = Routing_table.remove t.table pdiddy }

let lookup ~k t =
  Routing_table.find t.table (prefix_digit ~me:t.me t.b k)

