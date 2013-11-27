open Core.Std

let keys = QCheck.Arbitrary.(list ~len:small_int Test_lib.key_hexstring_gen)

let many_keys min =
  QCheck.Arbitrary.(list
		      ~len:(int_range ~start:min ~stop:(min * 5))
		      Test_lib.key_hexstring_gen)

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

let rec find_closest hsearch = function
  | [] ->
    None
  | l::h::r::_ when String.compare h hsearch = 0 ->
    let lkey = Test_lib.key_of_hexstring l in
    let hkey = Test_lib.key_of_hexstring h in
    let rkey = Test_lib.key_of_hexstring r in
    Some (Pastry.Node.create
	    ~distance:0
	    ~k:(Pastry.Key.closest hkey (lkey, rkey))
	    ())
  | _::rest ->
    find_closest hsearch rest

let contains hme hsearch hs =
  let half    = 2 lsl 4 in
  let smaller =
    hs
    |> List.filter ~f:(fun h -> String.compare h hme < 0)
    |> List.sort ~cmp:String.compare
    |> List.rev
    |> Fn.flip List.take half
    |> List.rev
  in
  let larger  =
    hs
    |> List.filter ~f:(fun h -> String.compare h hme > 0)
    |> List.sort ~cmp:String.compare
    |> Fn.flip List.take half
  in
  let hkeys   =
    smaller @ [hme; hsearch] @ larger
  in
  let sorted  = List.sort ~cmp:String.compare hkeys in
  find_closest hsearch sorted

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

let route_prop =
  QCheck.mk_test
    ~n:1000
    ~name:"Route"
    ~pp:QCheck.PP.(triple keys_pp string string)
    QCheck.Arbitrary.(triple
			(* (many_keys 20) *)
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
      let kmenode = Pastry.Node.create ~distance:0 ~k:kme () in
      let router =
	List.fold_left
	  ~f:(fun acc node -> Pastry.Router.update ~node acc)
	  ~init:(Pastry.Router.create ~me:kmenode ~b:4)
	  ksnodes
      in
      let routed = Pastry.Router.route ~k:ksearch router in
      let contained = contains hme hsearch hs in
      let lookedup = lookup hme hsearch hs in
      printf "Me: %s\n" hme;
      printf "Search: %s\n" hsearch;
      printf "Routed: %s\n" (node_printer routed);
      printf "Contained: %s\n" (node_option_printer contained);
      printf "Lookedup: %s\n" (node_option_printer lookedup);
      (node_option_compare (Some routed) contained) ||
	(node_option_compare (Some routed) lookedup) ||
	(node_option_compare (Some routed) (Some kmenode)))


let props =
  [ route_prop
  ]

let _ =
  if QCheck.run_tests props then
    exit 0
  else
    exit 1
