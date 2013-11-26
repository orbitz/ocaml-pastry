open Core.Std

type 'a t = { me            : 'a Node.t
	    ; b             : int
	    ; leaf_set      : 'a Leaf_set.t
	    ; routing_table : 'a Routing_table.t
	    }

let create ~me ~b =
  { me
  ; b
  ; leaf_set      = Leaf_set.create ~me (2 * (2 lsl b))
  ; routing_table = Routing_table.create ~me:(Node.key me) ~b
  }

let update ~node t =
  let (evicted, leaf_set) = Leaf_set.update ~node t.leaf_set in
  let routing_table =
    match evicted with
      | None   ->
	t.routing_table
      | Some node -> begin
	match Routing_table.lookup ~k:(Node.key node) t.routing_table with
	  | None ->
	    Routing_table.add ~node t.routing_table
	  | Some n when Node.distance node < Node.distance n ->
	    Routing_table.add ~node t.routing_table
	  | Some _ ->
	    t.routing_table
      end
  in
  { t with leaf_set; routing_table }

let remove ~k t =
  { t with
    leaf_set      = Leaf_set.remove ~k t.leaf_set
  ; routing_table = Routing_table.remove ~k t.routing_table
  }

let route ~k t =
  match Leaf_set.contains ~k t.leaf_set with
    | Some n ->
      n
    | None   -> begin
      match Routing_table.lookup ~k t.routing_table with
	| Some n ->
	  n
	| None -> begin
	  (*
	   * If it's not found in either, then find the node that shares
	   * a key just as long, if not longer, than the current node.
	   * Otherwise route to self
	   *)
	  let local_prefix = Key.prefix ~b:t.b (Node.key t.me) k in
	  let nodes =
	    List.filter
	      ~f:(fun n ->
		Key.compare (Node.key t.me) (Node.key n) <> 0 &&
		  Key.prefix ~b:t.b (Node.key n) k >= local_prefix)
	      (Routing_table.nodes t.routing_table @ Leaf_set.nodes t.leaf_set)
	  in
	  match nodes with
	    | []   -> t.me
	    | n::_ -> n
	end
    end

let leaf_set t = t.leaf_set

let routing_table t = t.routing_table
