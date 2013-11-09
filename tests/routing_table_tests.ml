open Core.Std
open OUnit

let node_printer n =
  n
  |> Pastry.Node.key
  |> Pastry.Key.to_string
  |> Pastry.Hexstring.encode

let node_option_printer = function
  | Some n ->
    "Some " ^ node_printer n
  | None ->
    "None"

let node_option_compare x y =
  match (x, y) with
    | (Some n1, Some n2) ->
      let k1 = Pastry.Node.key n1 in
      let k2 = Pastry.Node.key n2 in
      Pastry.Key.compare k1 k2 = 0
    | (None, None) ->
      true
    | _ ->
      false

let to_key hash =
  Option.value_exn
    (Pastry.Key.of_string
       (Option.value_exn
	  (Pastry.Hexstring.decode hash)))

let test_key_lookup_found1 () =
  let me_hash   = "d3b07384d113edec49eaa6238ad5ff00" in
  let me        = to_key me_hash in
  let node_hash = String.copy me_hash in
  node_hash.[1] <- '4';
  let node = Pastry.Node.create ~distance:0 ~k:(to_key node_hash) () in
  let rt   =
    Pastry.Routing_table.create ~me ~b:4
    |> Pastry.Routing_table.add ~node
  in
  assert_equal
    ~printer:node_option_printer
    ~cmp:node_option_compare
    (Some node)
    (Pastry.Routing_table.lookup ~k:(Pastry.Node.key node) rt)

let test_key_lookup_notfound1 () =
  let me_hash = "0123456789abcdef0123456789abcdef" in
  let me = to_key me_hash in
  let node_hash = String.copy me_hash in
  node_hash.[0] <- '1';
  let node = Pastry.Node.create ~distance:0 ~k:(to_key node_hash) () in
  let rt   =
    Pastry.Routing_table.create ~me ~b:4
    |> Pastry.Routing_table.add ~node
  in
  let search_hash = String.copy node_hash in
  search_hash.[0] <- '2';
  let search = to_key search_hash in
  assert_equal
    ~printer:node_option_printer
    ~cmp:node_option_compare
    None
    (Pastry.Routing_table.lookup ~k:search rt)

let suite = "Pastry Base Tests"  >:::
  [ "Routing Table: Lookup Found 1"     >:: (test_key_lookup_found1)
  ; "Routing Table: Lookup Not Found 1" >:: (test_key_lookup_notfound1)
  ]

let _ = run_test_tt_main suite
