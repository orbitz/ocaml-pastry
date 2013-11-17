open Core.Std
open OUnit

let key_compare x y = Pastry.Key.compare x y = 0

let me_hash = "d3b07384d113edec49eaa6238ad5ff00"

let to_key hash =
  Option.value_exn
    (Pastry.Key.of_string
       (Option.value_exn
	  (Pastry.Hexstring.decode hash)))

let test_node_create () =
  let key  = to_key me_hash in
  let node = Pastry.Node.create ~distance:0 ~k:key () in
  assert_equal
    ~cmp:key_compare
    key
    (Pastry.Node.key node)

let suite = "Pastry Base Tests"  >:::
  [ "Node: Create" >:: (test_node_create)
  ]

let _ = run_test_tt_main suite
