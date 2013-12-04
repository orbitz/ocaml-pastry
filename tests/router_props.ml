open Core.Std

let b = 4
let leaf_set_size = 2 lsl 4

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

let lf_contains hme hsearch leaf_set =
  let sorted = List.sort ~cmp:String.compare ([hsearch; hme] @ leaf_set) in
  find_closest hsearch sorted

let rt_lookup hme hsearch routing_table =
  let search_prefix = Test_lib.hexstring_prefix hme hsearch in
  let routes =
    List.filter
      ~f:(fun h ->
	Test_lib.hexstring_prefix hsearch h > search_prefix &&
	  h.[search_prefix] = hsearch.[search_prefix])
      routing_table
  in
  match routes with
    | [] ->
      None
    | h::_ ->
      Some (Pastry.Node.create
	      ~distance:0
	      ~k:(Test_lib.key_of_hexstring h)
	      ())

let find_any hme hsearch leaf_set routing_table =
  let prefix  = Test_lib.hexstring_prefix hsearch hme in
  let ksearch = Test_lib.key_of_hexstring hsearch in
  let key =
    List.fold_left
      ~f:(fun closest h ->
	if Test_lib.hexstring_prefix h hme >= prefix then
	  Test_lib.hexstring_of_key
	    (Pastry.Key.closest
	       ksearch
	       (Test_lib.key_of_hexstring h, Test_lib.key_of_hexstring closest))
	else
	  closest)
      ~init:hme
      (leaf_set @ [hme] @ routing_table)
  in
  Some (Pastry.Node.create
	  ~distance:0
	  ~k:(Test_lib.key_of_hexstring key)
	  ())

let rec run_until_some default = function
  | [] ->
    default
  | f::rest -> begin
    match f () with
      | Some v -> v
      | None   -> run_until_some default rest
  end

let build_router hs hme =
  let kme     = Test_lib.key_of_hexstring hme in
  let kmenode = Pastry.Node.create ~distance:0 ~k:kme () in
  let router  = Pastry.Router.create ~me:kmenode ~b in
  List.fold_left
    ~f:(fun r h ->
      let kh = Test_lib.key_of_hexstring h in
      let khnode = Pastry.Node.create ~distance:0 ~k:kh () in
      Pastry.Router.update ~node:khnode r)
    ~init:router
    hs

let build_fake_router hs hme =
  let ((l, r), rt) =
    List.fold_left
      ~f:(fun ((l, r), rt) h ->
	match String.compare h hme with
	  | 0 ->
	    failwith "noidea"
	  | c when c < 0 ->
	    let l' = List.sort ~cmp:(Fn.flip String.compare) (h::l) in
	    let evicted = List.drop l' leaf_set_size in
	    let l' = List.sort ~cmp:String.compare (List.take l' leaf_set_size) in
	    ((l', r), evicted @ rt)
	  | _ ->
	    let r' = List.sort ~cmp:String.compare (h::r) in
	    let evicted = List.drop r' leaf_set_size in
	    let r' = List.sort ~cmp:String.compare (List.take r' leaf_set_size) in
	    ((l, r'), evicted @ rt))
      ~init:(([], []), [])
      hs
  in
  let pdiddy h hme =
    let prefix = Test_lib.hexstring_prefix h hme in
    (prefix, h.[prefix])
  in
  let rec remove_existing = function
    | [] -> []
    | h::hs ->
      let puffy = pdiddy h hme in
      let filtered =
	List.filter
	  ~f:(fun h -> puffy <> pdiddy h hme)
	  hs
      in
      h::(remove_existing filtered)
  in
  (l @ r, remove_existing (List.rev rt))

let fake_route hs hme hsearch (leaf_set, routing_table) =
  run_until_some
    (Pastry.Node.create ~distance:0 ~k:(Test_lib.key_of_hexstring hme) ())
    [ (fun () -> lf_contains hme hsearch leaf_set)
    ; (fun () -> rt_lookup hme hsearch routing_table)
    ; (fun () -> find_any hme hsearch leaf_set routing_table)
    ]

let route_prop =
  QCheck.mk_test
    ~n:10000
    ~name:"Route"
    ~pp:QCheck.PP.(triple keys_pp string string)
    QCheck.Arbitrary.(triple
			(many_keys 20)
			Test_lib.key_hexstring_gen
			Test_lib.key_hexstring_gen)
    (fun (hs, hme, hsearch) ->
      let ksearch     = Test_lib.key_of_hexstring hsearch in
      let router      = build_router hs hme in
      let fake_router = build_fake_router hs hme in
      let real_routed = Pastry.Router.route ~k:ksearch router in
      let fake_routed = fake_route hs hme hsearch fake_router in
      let r = Pastry.(Key.compare (Node.key real_routed) (Node.key fake_routed)) in
      if r <> 0 then begin
	(* Pretty handy for debugging *)
	let (leaf_set, routing_table) = fake_router in
      	printf "me: %s\n" hme;
      	printf "search: %s\n" hsearch;
      	printf "routed: %s\n" (node_printer real_routed);
      	printf "fake: %s\n" (node_printer fake_routed);
	printf "lf_contains: %s\n" (node_option_printer (lf_contains hme hsearch leaf_set));
	printf "real contains: %s\n" (node_option_printer Pastry.(Leaf_set.contains ~k:(Test_lib.key_of_hexstring hsearch) (Router.leaf_set router)));
	printf "rt_lookup: %s\n" (node_option_printer (rt_lookup hme hsearch routing_table));
	printf "real rt_lookup: %s\n" (node_option_printer Pastry.(Routing_table.lookup ~k:(Test_lib.key_of_hexstring hsearch) (Router.routing_table router)));
	printf "find_any: %s\n" (node_option_printer (find_any hme hsearch leaf_set routing_table))
      end;
      Pastry.(Key.compare (Node.key real_routed) (Node.key fake_routed) = 0))

let routes_closer_prop =
  QCheck.mk_test
    ~n:10000
    ~name:"Routes Closer"
    ~pp:QCheck.PP.(triple keys_pp string string)
    QCheck.Arbitrary.(triple
			(many_keys 20)
			Test_lib.key_hexstring_gen
			Test_lib.key_hexstring_gen)
    (fun (hs, hme, hsearch) ->
      let ksearch = Test_lib.key_of_hexstring hsearch in
      let kme     = Test_lib.key_of_hexstring hme in
      let router  = build_router hs hme in
      let routed  = Pastry.Router.route ~k:ksearch router in
      let krouted = Pastry.Node.key routed in
      (* It either equals krouted or it is equidistant *)
      let closer = Pastry.(Key.compare (Key.closest ksearch (krouted, kme)) krouted = 0) in
      if not closer then begin
	let kclosest = Pastry.(Key.closest ksearch (krouted, kme)) in
	printf "hme: %s\n" hme;
	printf "hsearch: %s\n" hsearch;
	printf "krouted: %s\n" (Test_lib.hexstring_of_key krouted);
	printf "kclosest: %s\n" (Test_lib.hexstring_of_key kclosest);
      end;
      closer)


let props =
  [ route_prop
  ; routes_closer_prop
  ]

let _ =
  if QCheck.run_tests props then
    exit 0
  else
    exit 1
