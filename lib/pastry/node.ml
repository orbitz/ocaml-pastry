open Core.Std

type 'a t = { a : 'a
	    ; k : Key.t
	    ; d : int
	    }

let create ~distance ~k a =
  { d = distance; k; a }

let distance t = t.d

let set_distance d t = { t with d }

let key t = t.k

let closest k (t1, t2) =
  match Key.closest k (t1.k, t2.k) with
    | c when Key.compare c t1.k = 0 ->
      t1
    | _ ->
      t2

let find_closest k = function
  | [] ->
    None
  | n::nodes ->
    Some (List.fold_left
	    ~f:(fun close_node n -> closest k (close_node, n))
	    ~init:n
	    nodes)

let of_t t = t.a

