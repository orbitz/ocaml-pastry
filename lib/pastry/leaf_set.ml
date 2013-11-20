open Core.Std

type 'a t = { me    : 'a Node.t
	    ; size  : int
	    ; nodes : 'a Node.t list
	    }

let compare_by_key n1 n2 =
  Key.compare (Node.key n1) (Node.key n2)

let doesn't_equal n1 n2 =
  compare_by_key n1 n2 <> 0

let between k (k1, k2) =
  Key.compare k k1 >= 0 && Key.compare k k2 <= 0

let closest_node k (n1, n2) =
  let k_n1 = Node.key n1 in
  let k_n2 = Node.key n2 in
  match Key.closest k (k_n1, k_n2) with
    | k' when Key.compare k' k_n1 = 0 -> n1
    | k' when Key.compare k' k_n2 = 0 -> n2
    | _                               -> failwith "impossible"

let rec find_closest k = function
  (* Nodes must be sorted in descending order *)
  | [] | [_] ->
    None
  | n1::n2::nodes when between k (Node.key n1, Node.key n2) ->
      Some (closest_node k (n1, n2))
  | _::nodes ->
    find_closest k nodes

let create ~me size =
  { me; size; nodes = [me] }

let update node t =
  let nodes = List.sort ~cmp:compare_by_key (node::t.nodes) in
  let smaller =
    List.take_while
      ~f:(fun n -> Key.compare (Node.key n) (Node.key t.me) < 0)
      nodes
  in
  let larger =
    List.drop_while
      ~f:(fun n -> Key.compare (Node.key n) (Node.key t.me) <= 0)
      nodes
  in
  let nodes =
    List.rev (List.take (List.rev smaller) t.size) @ [t.me] @ List.take larger t.size
  in
  {t with nodes = nodes }

let remove node t =
  let nodes = List.filter ~f:(doesn't_equal node) t.nodes in
  { t with nodes = nodes }

let nodes t = t.nodes

let contains k t =
  find_closest k t.nodes
