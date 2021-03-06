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

let test_key_lookup_notfound2 () =
  let me_hash     = "6F7761766C6377627562726B70626564" in
  let search_hash = "696E797274677378627468777961636A" in
  let hashes      = [ "726B6A6A73646F776863786E636D6564"
		    ; "75767069786D616E656D787174677172"
		    ; "6E77696D6E6D67686675627961717470"
		    ; "70766278706D7878746C616865616270"
		    ; "6F62696F75626D68737568676B6E6678"
		    ; "746B666D67786864646F6C6E6F737078"
		    ]
  in
  let me_key      = Test_lib.key_of_hexstring me_hash in
  let search_key  = Test_lib.key_of_hexstring search_hash in
  let rt =
    List.fold_left
      ~f:(fun rt h ->
	Pastry.Routing_table.add
	  ~node:(Pastry.Node.create
		   ~distance:0
		   ~k:(Test_lib.key_of_hexstring h)
		   ())
	  rt)
      ~init:(Pastry.Routing_table.create ~me:me_key ~b:4)
      hashes
  in
  assert_equal
    ~printer:node_option_printer
    ~cmp:node_option_compare
    None
    (Pastry.Routing_table.lookup ~k:search_key rt)

let test_nodes () =
  let me_hash = "0123456789abcdef0123456789abcdef" in
  let me = to_key me_hash in
  let node_hash = String.copy me_hash in
  node_hash.[0] <- '1';
  let node = Pastry.Node.create ~distance:0 ~k:(to_key node_hash) () in
  let rt   =
    Pastry.Routing_table.create ~me ~b:4
    |> Pastry.Routing_table.add ~node
  in
  assert_equal
    1
    (List.length (Pastry.Routing_table.nodes rt))

let suite = "Pastry Base Tests"  >:::
  [ "Routing Table: Lookup Found 1"     >:: (test_key_lookup_found1)
  ; "Routing Table: Lookup Not Found 1" >:: (test_key_lookup_notfound1)
  ; "Routing Table: Nodes"              >:: (test_nodes)
  ]

let _ = run_test_tt_main suite
