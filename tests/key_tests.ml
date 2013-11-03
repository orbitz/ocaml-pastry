open Core.Std
open OUnit

let all_zeros = String.make 16 '\000'

let test_key_create_success () =
  let res =
    match Pastry.Key.of_string all_zeros with
      | Some _ -> true
      | None   -> false
  in
  assert_equal
    true
    res

let test_key_create_short_failure () =
  assert_equal
    None
    (Pastry.Key.of_string "")

let test_key_create_long_failure () =
  assert_equal
    None
    (Pastry.Key.of_string "00000000000000000")

let test_key_prefix_all_same () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let v2 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    16
    (Pastry.Key.prefix ~b:8 v1 v2)

let test_key_prefix_none_same () =
  let v1 = Option.value_exn (Pastry.Key.of_string "1000000000000000") in
  let v2 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    0
    (Pastry.Key.prefix ~b:8 v1 v2)

let test_key_prefix_eq_b_1 () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let v2 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    128
    (Pastry.Key.prefix ~b:1 v1 v2)

let test_key_prefix_eq_b_4 () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let v2 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    (128/4)
    (Pastry.Key.prefix ~b:4 v1 v2)

let test_key_prefix_neq_b_4 () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let s  = String.copy all_zeros in
  s.[3] <- '\255';
  let v2 = Option.value_exn (Pastry.Key.of_string s) in
  assert_equal
    ~printer:Int.to_string
    6
    (Pastry.Key.prefix ~b:4 v1 v2)

let test_key_prefix_neq_b_4_another () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let s  = String.copy all_zeros in
  s.[3] <- '\001';
  let v2 = Option.value_exn (Pastry.Key.of_string s) in
  assert_equal
    ~printer:Int.to_string
    7
    (Pastry.Key.prefix ~b:4 v1 v2)


let suite = "Pastry Base Tests"  >:::
  [ "Key: Create Success"       >:: (test_key_create_success)
  ; "Key: Create Short Failure" >:: (test_key_create_short_failure)
  ; "Key: Create Long Failure"  >:: (test_key_create_long_failure)
  ; "Key: Prefix All Same"      >:: (test_key_prefix_all_same)
  ; "Key: Prefix None Same"     >:: (test_key_prefix_none_same)
  ; "Key: Prefix Eq b = 1"      >:: (test_key_prefix_eq_b_1)
  ; "Key: Prefix Eq b = 4"      >:: (test_key_prefix_eq_b_4)
  ; "Key: Prefix Neq b = 4"     >:: (test_key_prefix_neq_b_4)
  ; "Key: Prefix Neq b = 4 2"   >:: (test_key_prefix_neq_b_4_another)
  ]

let _ = run_test_tt_main suite
