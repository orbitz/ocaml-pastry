open Core.Std

(* let sort_by_closest me resp = *)
(*   List.sort *)
(*     ~cmp:(fun x y -> *)
(*       let n1 = x.Msg.Announce_resp.node in *)
(*       let n2 = y.Msg.Announce_resp.node in *)
(*       let closest = Node.closest (Node.key me) (n1,  n2) in *)
(*       if Key.compare (Node.key n1) (Node.key n2) = 0 then *)
(* 	0 *)
(*       else if Key.compare (Node.key closest) (Node.key n1) = 0 then *)
(* 	1 *)
(*       else *)
(* 	-1) *)
(*     resp *)

let add_leaf_set = Fn.id

let add_routing_table = Fn.id

let return_contact_nodes (_, contact_nodes) =
  contact_nodes

let router_nodes ~me resp =
  (resp, [])
  |> add_leaf_set
  |> add_routing_table
  |> return_contact_nodes
