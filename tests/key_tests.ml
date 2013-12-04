open Core.Std
open OUnit

let key_compare k1 k2 = Pastry.Key.compare k1 k2 = 0

let string_of_key = Fn.compose Pastry.Hexstring.encode Pastry.Key.to_string

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
    (Pastry.Key.of_string (String.make 15 '\000'))

let test_key_prefix_none_same () =
  let v1 = Option.value_exn (Pastry.Key.of_string "1000000000000000") in
  let v2 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    ~printer:Int.to_string
    0
    (Pastry.Key.prefix ~b:4 v1 v2)

let test_key_prefix_eq_b_4 () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let v2 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    ~printer:Int.to_string
    (128/4)
    (Pastry.Key.prefix ~b:4 v1 v2)

let test_key_prefix_neq_b_4 () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let s  = String.copy all_zeros in
  s.[0] <- '\001';
  let v2 = Option.value_exn (Pastry.Key.of_string s) in
  assert_equal
    ~printer:Int.to_string
    1
    (Pastry.Key.prefix ~b:4 v1 v2)

let test_key_prefix_neq_b_4_another () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let s  = String.copy all_zeros in
  s.[7] <- '\001';
  let v2 = Option.value_exn (Pastry.Key.of_string s) in
  assert_equal
    ~printer:Int.to_string
    15
    (Pastry.Key.prefix ~b:4 v1 v2)

let test_key_digit_zero () =
  let v1 = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    ~printer:Int.to_string
    0
    (Pastry.Key.digit ~b:4 0 v1)

let test_key_digit_low_bits () =
  let s = String.copy all_zeros in
  s.[1] <- '\001';
  let v1 = Option.value_exn (Pastry.Key.of_string s) in
  assert_equal
    ~printer:Int.to_string
    1
    (Pastry.Key.digit ~b:4 3 v1)

let test_key_digit_high_bits () =
  let s = String.copy all_zeros in
  s.[0] <- '\001';
  let v1 = Option.value_exn (Pastry.Key.of_string s) in
  assert_equal
    ~printer:Int.to_string
    1
    (Pastry.Key.digit ~b:4 1 v1)

let test_key_digit_end () =
  let s = String.copy all_zeros in
  s.[15] <- '\001';
  let v1 = Option.value_exn (Pastry.Key.of_string s) in
  assert_equal
    ~printer:Int.to_string
    1
    (Pastry.Key.digit ~b:4 31 v1)

let test_key_closest1 () =
  let k = Option.value_exn (Pastry.Key.of_string all_zeros) in
  assert_equal
    ~printer:string_of_key
    ~cmp:key_compare
    k
    (Pastry.Key.closest k (k, k))

let test_key_closest2 () =
  let farther_s = String.copy all_zeros in
  let closer_s  = String.copy all_zeros in

  farther_s.[0]                         <- '\001';
  closer_s.[String.length closer_s - 1] <- '\001';

  let k       = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let farther = Option.value_exn (Pastry.Key.of_string farther_s) in
  let closer  = Option.value_exn (Pastry.Key.of_string closer_s) in
  assert_equal
    ~printer:string_of_key
    ~cmp:key_compare
    closer
    (Pastry.Key.closest k (closer, farther))

let test_key_closest3 () =
  let farther_s = String.copy all_zeros in
  let closer_s  = String.copy all_zeros in

  farther_s.[0]                         <- '\001';
  closer_s.[String.length closer_s - 1] <- '\001';

  let k       = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let farther = Option.value_exn (Pastry.Key.of_string farther_s) in
  let closer  = Option.value_exn (Pastry.Key.of_string closer_s) in
  assert_equal
    ~printer:string_of_key
    ~cmp:key_compare
    closer
    (Pastry.Key.closest k (farther, closer))

let test_key_closest4 () =
  let farther_s = String.copy all_zeros in
  let closer_s  = String.copy all_zeros in

  farther_s.[0] <- '\001';

  let k       = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let farther = Option.value_exn (Pastry.Key.of_string farther_s) in
  let closer  = Option.value_exn (Pastry.Key.of_string closer_s) in
  assert_equal
    ~printer:string_of_key
    ~cmp:key_compare
    closer
    (Pastry.Key.closest k (closer, farther))

let test_key_closest5 () =
  let farther_s = String.copy all_zeros in
  let closer_s  = String.copy all_zeros in

  farther_s.[String.length farther_s - 1] <- '\002';
  closer_s.[String.length closer_s - 1]   <- '\001';

  let k       = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let farther = Option.value_exn (Pastry.Key.of_string farther_s) in
  let closer  = Option.value_exn (Pastry.Key.of_string closer_s) in
  assert_equal
    ~printer:string_of_key
    ~cmp:key_compare
    closer
    (Pastry.Key.closest k (farther, closer))

let test_key_closest6 () =
  let farther_s = String.copy all_zeros in
  let closer_s  = String.copy all_zeros in

  farther_s.[0] <- '\002';
  closer_s.[0]   <- '\001';

  let k       = Option.value_exn (Pastry.Key.of_string all_zeros) in
  let farther = Option.value_exn (Pastry.Key.of_string farther_s) in
  let closer  = Option.value_exn (Pastry.Key.of_string closer_s) in
  assert_equal
    ~printer:string_of_key
    ~cmp:key_compare
    closer
    (Pastry.Key.closest k (farther, closer))

let test_key_closest7 () =
  let k_of_h = Test_lib.key_of_hexstring in
  let k1     = k_of_h "6E7779707679686566786D6373797763" in
  let k2     = k_of_h "686A676B727264646A6E656B726B6D74" in
  let k3     = k_of_h "69707470726A69716C70627578747469" in
  assert_equal
    ~printer:string_of_key
    ~cmp:key_compare
    k3
    (Pastry.Key.closest k1 (k2, k3))


let suite = "Pastry Base Tests"  >:::
  [ "Key: Create Success"       >:: (test_key_create_success)
  ; "Key: Create Short Failure" >:: (test_key_create_short_failure)
  ; "Key: Create Long Failure"  >:: (test_key_create_long_failure)
  ; "Key: Prefix None Same"     >:: (test_key_prefix_none_same)
  ; "Key: Prefix Eq b = 4"      >:: (test_key_prefix_eq_b_4)
  ; "Key: Prefix Neq b = 4"     >:: (test_key_prefix_neq_b_4)
  ; "Key: Prefix Neq b = 4 2"   >:: (test_key_prefix_neq_b_4_another)
  ; "Key: Digit Zero"           >:: (test_key_digit_zero)
  ; "Key: Digit Low Bits"       >:: (test_key_digit_low_bits)
  ; "Key: Digit High Bits"      >:: (test_key_digit_high_bits)
  ; "Key: Digit End"            >:: (test_key_digit_end)
  ; "Key: Closest 1"            >:: (test_key_closest1)
  ; "Key: Closest 2"            >:: (test_key_closest2)
  ; "Key: Closest 3"            >:: (test_key_closest3)
  ; "Key: Closest 4"            >:: (test_key_closest4)
  ; "Key: Closest 5"            >:: (test_key_closest5)
  ; "Key: Closest 6"            >:: (test_key_closest6)
  ; "Key: Closest 7"            >:: (test_key_closest7)
  ]

let _ = run_test_tt_main suite
