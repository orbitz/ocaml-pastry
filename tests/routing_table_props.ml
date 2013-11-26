open Core.Std

let keys = QCheck.Arbitrary.(list ~len:small_int Test_lib.key_hexstring_gen)

let keys_pp = QCheck.PP.(list string)

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

let rec lookup_matching prefix hsearch = function
  | [] ->
    None
  | h::_ when Test_lib.hexstring_prefix h hsearch > prefix ->
    Some (Pastry.Node.create
	    ~distance:0
	    ~k:(Test_lib.key_of_hexstring h)
	    ())
  | _::rest ->
    lookup_matching prefix hsearch rest

let lookup hme hsearch hs =
  let search_prefix = Test_lib.hexstring_prefix hme hsearch in
  let matching_hexstrings =
    List.filter
      ~f:(fun h -> Test_lib.hexstring_prefix hme h = search_prefix)
      hs
  in
  (*
   * Reverse the list because the last one to be inserted into the
   * routing table will be what we find,
   *)
  lookup_matching search_prefix hsearch (List.rev matching_hexstrings)

let lookup_prop =
  QCheck.mk_test
    ~n:1000
    ~name:"Lookup"
    ~pp:QCheck.PP.(triple keys_pp string string)
    QCheck.Arbitrary.(triple
			keys
			Test_lib.key_hexstring_gen
			Test_lib.key_hexstring_gen)
    (fun (hs, hme, hsearch) ->
      let ks      = List.map ~f:Test_lib.key_of_hexstring hs in
      let kme     = Test_lib.key_of_hexstring hme in
      let ksearch = Test_lib.key_of_hexstring hsearch in
      let ksnodes =
	List.map
	  ~f:(fun k -> Pastry.Node.create ~distance:0 ~k ())
	  ks
      in
      let routing_table =
	List.fold_left
	  ~f:(fun rt node -> Pastry.Routing_table.add ~node rt)
	  ~init:(Pastry.Routing_table.create ~me:kme ~b:4)
	  ksnodes
      in
      node_option_compare
	(Pastry.Routing_table.lookup ~k:ksearch routing_table)
	(lookup hme hsearch hs))

let props =
  [ lookup_prop
  ]

let _ =
  if QCheck.run_tests props then
    exit 0
  else
    exit 1
