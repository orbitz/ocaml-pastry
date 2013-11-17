open Core.Std

open OUnit

let key_hexstring_gen =
  QCheck.Arbitrary.(lift
		      Pastry.Hexstring.encode
		      (string_len (return 16)))

let key_of_hexstring h =
  Option.value_exn
    (Pastry.Key.of_string
       (Option.value_exn
	  (Pastry.Hexstring.decode h)))

let hexstring_of_key =
  Fn.compose
    Pastry.Hexstring.encode
    Pastry.Key.to_string

let hexstring_prefix h1 h2 =
  let rec prefix = function
    | n when n < String.length h1 && n < String.length h2 && h1.[n] = h2.[n] ->
      prefix (n + 1)
    | n ->
      n
  in
  (*
   * Divide by 4 because we operate on 16bit digits and each hex base represents
   * 1/4th a digit
   *)
  (prefix 0) / 4

let prefix_prop =
  QCheck.mk_test
    ~n:10000
    ~name:"Prefix"
    ~pp:QCheck.PP.(pair string string)
    QCheck.Arbitrary.(pair key_hexstring_gen key_hexstring_gen)
    (fun (h1, h2) ->
	let hprefix = hexstring_prefix h1 h2 in
	let kprefix =
	  Pastry.Key.prefix ~b:4 (key_of_hexstring h1) (key_of_hexstring h2)
	in
	hprefix = kprefix)

let closest_commutative_prop =
  QCheck.mk_test
    ~n:1000
    ~name:"Closest Commutative"
    ~pp:QCheck.PP.(triple string string string)
    QCheck.Arbitrary.(triple
			key_hexstring_gen
			key_hexstring_gen
			key_hexstring_gen)
    (fun (h1, h2, h3) ->
      let k1 = key_of_hexstring h1 in
      let k2 = key_of_hexstring h2 in
      let k3 = key_of_hexstring h3 in
      Pastry.Key.(
	to_string (closest k1 (k2, k3)) = to_string (closest k1 (k3, k2))))

let props =
  [ prefix_prop
  ; closest_commutative_prop
  ]

let _ =
  if QCheck.run_tests props then
    exit 0
  else
    exit 1
