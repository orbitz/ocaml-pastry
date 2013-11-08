open Core.Std
open OUnit


let test_encode () =
  assert_equal
    ~printer:Fn.id
    "12"
    (Pastry.Hexstring.encode "\x12")

let test_decode () =
  assert_equal
    (Some "\x34")
    (Pastry.Hexstring.decode (Pastry.Hexstring.encode "\x34"))

let test_encode_multi () =
  assert_equal
    ~printer:Fn.id
    "FE1AA5"
    (Pastry.Hexstring.encode "\xfe\x1a\xa5")

let test_decode_multi () =
  assert_equal
    (Some "\xff\x1a\x55")
    (Pastry.Hexstring.decode (Pastry.Hexstring.encode "\xff\x1a\x55"))

let test_decode_fail () =
  assert_equal
    None
    (Pastry.Hexstring.decode "test")

let suite = "Pastry Base Tests"  >:::
  [ "Hexstring: Encode"       >:: (test_encode)
  ; "Hexstring: Decode"       >:: (test_decode)
  ; "Hexstring: Encode Multi" >:: (test_encode_multi)
  ; "Hexstring: Decode Multi" >:: (test_decode_multi)
  ; "Hexstring: Decode Fail"  >:: (test_decode_fail)
  ]

let _ = run_test_tt_main suite
