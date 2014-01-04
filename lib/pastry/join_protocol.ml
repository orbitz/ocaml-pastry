open Core.Std

let sort_by_closest me resp =
  List.sort
    ~cmp:(fun x y ->
      let n1 = Router.me x in
      let n2 = Router.me y in
      let closest = Node.closest (Node.key me) (n1,  n2) in
      if Key.compare (Node.key n1) (Node.key n2) = 0 then
	0
      else if Key.compare (Node.key closest) (Node.key n1) = 0 then
	1
      else
	-1)
    resp

let leaf_set me (resp, nodes) =
  match sort_by_closest me resp with
    | [] ->
      (* This is an odd place to be but...whatever *)
      (resp, nodes)
    | router::_ ->
      let leaf_set = Leaf_set.nodes (Router.leaf_set router)
      in
      (resp, nodes @ leaf_set @ [Router.me router])

let extract_nodes_with_prefix me router =
  let router_me = Router.me router in
  let prefix    = Key.prefix (Node.key me) (Node.key router_me) in
  let rt_nodes  = Routing_table.nodes (Router.routing_table router) in
  let prefix_nodes =
    List.filter
      ~f:(fun n -> Key.prefix (Node.key n) (Node.key router_me) <= prefix)
      rt_nodes
  in
  router_me::prefix_nodes

let routing_table me (resp, nodes) =
  let relevant_nodes =
    List.concat_map
      ~f:(extract_nodes_with_prefix me)
      resp
  in
  (resp, nodes @ relevant_nodes)

let return_contact_nodes (_, contact_nodes) =
  contact_nodes

let router_nodes ~me resp =
  (resp, [])
  |> leaf_set me
  |> routing_table me
  |> return_contact_nodes
