open Core.Std

type 'a t = { me      : 'a Node.t
	    ; size    : int
	    ; larger  : 'a Node.t list
	    ; smaller : 'a Node.t list
	    }

let sort_by_key_desc n1 n2 =
  Key.compare (Node.key n2) (Node.key n1)

let doesn't_equal n1 n2 =
  sort_by_key_desc n1 n2 = 0

let between k (k1, k2) =
  Key.compare k1 k >= 0 && Key.compare k k2 <= 0

let closest_node k (n1, n2) =
  let k_n1 = Node.key n1 in
  let k_n2 = Node.key n2 in
  match Key.closest k (k_n1, k_n2) with
    | k' when Key.compare k' k_n1 = 0 -> n1
    | k' when Key.compare k' k_n2 = 0 -> n2
    | _                               -> failwith "impossible"

let rec find_closest k me = function
  (* Nodes must be sorted in descending order *)
  | [] when Key.compare k (Node.key me) = 0 ->
    Some me
  | [] ->
    None
  | [n] ->
    Some (closest_node k (me, n))
  | n1::n2::nodes when Key.compare (Node.key n1) k >= 0 -> begin
    if between k (Node.key n1, Node.key n2) then
      Some (closest_node k (n1, n2))
    else
      find_closest k me (n2::nodes)
  end
  | _ ->
    None

let sort_and_take size nodes =
  nodes
  |> List.sort ~cmp:sort_by_key_desc
  |> Fn.flip List.take size

let run_with_smaller_or_larger cmp ~e ~s ~l =
  match cmp with
    | 0            -> e ()
    | n when n < 0 -> s ()
    | n            -> l ()

let create ~me size =
  { me; size; larger = []; smaller = [] }

let update node t =
  run_with_smaller_or_larger
    (Key.compare (Node.key node) (Node.key t.me))
    ~e:(fun () -> t)
    ~s:(fun () -> { t with smaller = sort_and_take t.size (node::t.smaller) })
    ~l:(fun () -> { t with larger = sort_and_take t.size (node::t.larger) })

let remove node t =
  run_with_smaller_or_larger
    (Key.compare (Node.key node) (Node.key t.me))
    ~e:(fun () -> t)
    ~s:(fun () -> { t with smaller = List.filter ~f:(doesn't_equal node) t.smaller })
    ~l:(fun () -> { t with larger = List.filter ~f:(doesn't_equal node) t.larger })

let nodes t = t.smaller @ t.larger

let contains k t =
  run_with_smaller_or_larger
    (Key.compare k (Node.key t.me))
    ~e:(fun () -> Some t.me)
    ~s:(fun () -> find_closest k t.me t.smaller)
    ~l:(fun () -> find_closest k t.me t.larger)
