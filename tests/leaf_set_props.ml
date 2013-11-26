open Core.Std

let keys = QCheck.Arbitrary.(list ~len:small_int Test_lib.key_hexstring_gen)

let many_keys min = QCheck.Arbitrary.(list ~len:(int_range ~start:min ~stop:(min * 2)) Test_lib.key_hexstring_gen)

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
  let half    = List.length hs / 2 in
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

let contains_prop =
  QCheck.mk_test
    ~n:1000
    ~name:"Contains"
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
      let kmenode     = Pastry.Node.create ~distance:0 ~k:kme () in
      let leaf_set    =
	List.fold_left
	  ~f:(fun acc n -> snd (Pastry.Leaf_set.update n acc))
	  ~init:(Pastry.Leaf_set.create ~me:kmenode (List.length hs / 2))
	  ksnodes
      in
      node_option_compare
	(Pastry.Leaf_set.contains ksearch leaf_set)
	(contains hme hsearch hs))

let evicted_prop =
  QCheck.mk_test
    ~n:1000
    ~name:"Evicted"
    ~pp:QCheck.PP.(triple keys_pp string string)
    QCheck.Arbitrary.(triple
			(many_keys 10)
			Test_lib.key_hexstring_gen
			Test_lib.key_hexstring_gen)
    (fun (hs, hme, hlast) ->
      let size    = List.length hs / 2 in
      let ks      = List.map ~f:Test_lib.key_of_hexstring hs in
      let kme     = Test_lib.key_of_hexstring hme in
      let klast   = Test_lib.key_of_hexstring hlast in
      let ksnodes =
	List.map
	  ~f:(fun k -> Pastry.Node.create ~distance:0 ~k ())
	  ks
      in
      let kmenode     = Pastry.Node.create ~distance:0 ~k:kme () in
      let klastnode   = Pastry.Node.create ~distance:0 ~k:klast () in
      let leaf_set    =
	List.fold_left
	  ~f:(fun acc n -> snd (Pastry.Leaf_set.update n acc))
	  ~init:(Pastry.Leaf_set.create ~me:kmenode size)
	  ksnodes
      in
      let (evicted, _) = Pastry.Leaf_set.update klastnode leaf_set in
      match String.compare hme hlast with
	| 0 -> true
	| c -> begin
	  let nodes =
	    List.filter
	      ~f:(fun h -> String.compare hme h = c)
	      hs
	  in
	  (List.length nodes < size && Option.is_none evicted) ||
	    Option.is_some evicted
	end)

let props =
  [ contains_prop
  ; evicted_prop
  ]

let _ =
  if QCheck.run_tests props then
    exit 0
  else
    exit 1
