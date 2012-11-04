open Core.Std

type t = (Int64.t * int)

let now () =
  let parts = Float.modf (Time.to_float (Time.now ())) in
  ( Float.Parts.integral parts
  , Float.to_int (Float.Parts.fractional parts * 1000.)
  )

let seconds (s, _) = s

let milliseconds (_, ms) = ms

let compare (s1, ms1) (s2, ms2) =
  let c1 = compare s1 s2 in
  if c1 = 0 then
    compare ms1 ms2
  else
    c1

