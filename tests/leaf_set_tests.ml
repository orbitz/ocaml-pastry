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

let test_leaf_set_contains leafs me_hash search_hash found_hash () =
  let me_key      = Test_lib.key_of_hexstring me_hash in
  let me          = Pastry.Node.create ~distance:0 ~k:me_key () in
  let leaf_set    =
    List.fold_left
      ~f:(fun acc h ->
	let key = Test_lib.key_of_hexstring h in
	let node = Pastry.Node.create ~distance:0 ~k:key () in
	Pastry.Leaf_set.update node acc)
      ~init:(Pastry.Leaf_set.create ~me (List.length leafs / 2))
      leafs
  in
  let search      = Test_lib.key_of_hexstring search_hash in
  let found_key   = Test_lib.key_of_hexstring found_hash in
  let found_node  = Pastry.Node.create ~distance:0 ~k:found_key () in
  assert_equal
    ~printer:node_option_printer
    ~cmp:node_option_compare
    (Some found_node)
    (Pastry.Leaf_set.contains search leaf_set)

let me_hash_contains1     = "6C6D746C70677171676379747363666F"
let search_hash_contains1 = "6B776D6C736C6E75677372656867726B"
let found_hash_contains1  = "6C636B746979756A68706C627461686C"
let leafs_contains1 = [ "676178616F6F6E676A64617766777663"
		      ; "6872616871757171796B766E73746B69"
		      ; "74636C6B6F65697772726C75656B656F"
		      ; "7367746B667362747264647275657073"
		      ; "78776E6569726D6267697662616D626D"
		      ; "6F69666C79736D64767361687877656F"
		      ; "627075617469747570696A636A696E70"
		      ; "64707171756F7874736B657777747568"
		      ; "636C646965666E6D776E6E6173736D69"
		      ; "626A6F7165786F75726C69797975746C"
		      ; "76776D6A71646974776D6B7762717773"
		      ; "6F6C77677864766278636E616F666A6B"
		      ; "777374796873746371766E6776736E75"
		      ; "7369616F656F6D78616C6E77676E7368"
		      ; "6C636B746979756A68706C627461686C"
		      ; "62796A696664717578696C6B7064616B"
		      ; "62626D636A6473737164676B7172676A"
		      ; "667261676F6B65716A6D71746C646479"
		      ; "766F776F77626376766A7974746C6473"
		      ; "716B75776D73696E6E6F706B6279746F"
		      ; "676E77786C656677696F646364647378"
		      ; "6371686A786163637976686A73756578"
		      ; "65796A65737368756C686D6C67756B6C"
		      ]

let me_hash_contains2     = "70626D676162696B71616F746E6E6B74"
let search_hash_contains2 = "6A77626F626D786C7175666C75617063"
let found_hash_contains2  = "6B656E6D736366767072787272647071"
let leafs_contains2 = [ "6E75707065706C796166616B65636271"
		      ; "626276786A6B667463676A61736D7179"
		      ; "70716E6F6E696D776A67767764757475"
		      ; "61657262746A716178636D6E636F6371"
		      ; "726E676F70776D76757979736B666F75"
		      ; "636E6E63747868656E6D6B706C6A6E66"
		      ; "74796F72617574766377747978796279"
		      ; "7068636865646A687062656963726464"
		      ; "6B79667966676C686E72786662776F75"
		      ; "6978666C736E6B776B6A726767656978"
		      ; "6663616B747763796B78637670657474"
		      ; "736865766A77646F666879716A647372"
		      ; "70697865637877656B67696678686479"
		      ; "676F6A7274786A637179706D6B626866"
		      ; "64686462636177646E767263706E7863"
		      ; "6B656E6D736366767072787272647071"
		      ; "7465767670616861767261626F646566"
		      ; "737278676476726D6A6D6877766E626B"
		      ; "707068626873686476786F6C64616F69"
		      ; "766A666F717361647572627479616362"
		      ; "766F72687977687064666E796A6D6B63"
		      ; "6B6A78706A6D73746C74746C6872616A"
		      ; "766B6777696A696A7162796B6C677966"
		      ]


let test_leaf_set_contains1 =
  test_leaf_set_contains
    leafs_contains1
    me_hash_contains1
    search_hash_contains1
    found_hash_contains1

let test_leaf_set_contains2 =
  test_leaf_set_contains
    leafs_contains2
    me_hash_contains2
    search_hash_contains2
    found_hash_contains2

let suite = "Pastry Base Tests"  >:::
  [ "Leaf Set: Empty Contains" >:: (test_leaf_set_empty_contains1)
  ; "Leaf Set: Contains 1"     >:: (test_leaf_set_contains1)
  ; "Leaf Set: Contains 2"     >:: (test_leaf_set_contains2)
  ]

let _ = run_test_tt_main suite
