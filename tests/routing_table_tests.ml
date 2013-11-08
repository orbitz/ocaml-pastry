open Core.Std
open OUnit

let node_option_compare x y =
  match (x, y) with
    | (Some n1, Some n2) ->
      let k1 = Pastry.Node.key n1 in
      let k2 = Pastry.Node.key n2 in
      Pastry.Key.compare k1 k2 = 0
    | _ ->
      false

let me_hash = "d3b07384d113edec49eaa6238ad5ff00"

let to_key hash =
  Option.value_exn
    (Pastry.Key.of_string
       (Option.value_exn
	  (Pastry.Hexstring.decode hash)))

let test_key_lookup_found1 () =
  let me = to_key me_hash in
  let node_hash = String.copy me_hash in
  node_hash.[1] <- '4';
  let node = Pastry.Node.create ~distance:0 ~k:(to_key node_hash) () in
  let rt   =
    Pastry.Routing_table.create ~me ~b:4
    |> Pastry.Routing_table.add ~node
  in
  assert_equal
    ~cmp:node_option_compare
    (Some node)
    (Pastry.Routing_table.lookup ~k:(Pastry.Node.key node) rt)

let suite = "Pastry Base Tests"  >:::
  [ "Routing Table: Lookup Found 1" >:: (test_key_lookup_found1)
  ]

let _ = run_test_tt_main suite
