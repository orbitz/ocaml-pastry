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

let me_hash = "d3b07384d113edec49eaa6238ad5ff00"

let to_key hash =
  Option.value_exn
    (Pastry.Key.of_string
       (Option.value_exn
	  (Pastry.Hexstring.decode hash)))

let test_leaf_set_empty_contains1 () =
  let me          = Pastry.Node.create ~distance:0 ~k:(to_key me_hash) () in
  let leaf_set    = Pastry.Leaf_set.create ~me 0 in
  let search_hash = String.copy me_hash in
  search_hash.[0] <- '2';
  let search      = to_key search_hash in
  assert_equal
    ~printer:node_option_printer
    ~cmp:node_option_compare
    None
    (Pastry.Leaf_set.contains search leaf_set)

let suite = "Pastry Base Tests"  >:::
  [ "Leaf Set: Empty Contains" >:: (test_leaf_set_empty_contains1)
  ]

let _ = run_test_tt_main suite
