open Core.Std

let prefix_prop =
  QCheck.mk_test
    ~n:10000
    ~name:"Prefix"
    ~pp:QCheck.PP.(pair string string)
    QCheck.Arbitrary.(pair Test_lib.key_hexstring_gen Test_lib.key_hexstring_gen)
    (fun (h1, h2) ->
      let hprefix = Test_lib.hexstring_prefix h1 h2 in
      let kprefix =
	Pastry.Key.prefix
	  ~b:4
	  (Test_lib.key_of_hexstring h1)
	  (Test_lib.key_of_hexstring h2)
      in
      hprefix = kprefix)

let closest_commutative_prop =
  QCheck.mk_test
    ~n:1000
    ~name:"Closest Commutative"
    ~pp:QCheck.PP.(triple string string string)
    QCheck.Arbitrary.(triple
			Test_lib.key_hexstring_gen
			Test_lib.key_hexstring_gen
			Test_lib.key_hexstring_gen)
    (fun (h1, h2, h3) ->
      let k1 = Test_lib.key_of_hexstring h1 in
      let k2 = Test_lib.key_of_hexstring h2 in
      let k3 = Test_lib.key_of_hexstring h3 in
      Pastry.Key.(
	to_string (closest k1 (k2, k3)) = to_string (closest k1 (k3, k2))))

let compare_props =
  QCheck.mk_test
    ~n:10000
    ~name:"Compare"
    ~pp:QCheck.PP.(pair string string)
    QCheck.Arbitrary.(pair Test_lib.key_hexstring_gen Test_lib.key_hexstring_gen)
    (fun (h1, h2) ->
      let k1 = Test_lib.key_of_hexstring h1 in
      let k2 = Test_lib.key_of_hexstring h2 in
      String.compare h1 h2 = Pastry.Key.compare k1 k2)

let props =
  [ prefix_prop
  ; closest_commutative_prop
  ; compare_props
  ]

let _ =
  if QCheck.run_tests props then
    exit 0
  else
    exit 1
