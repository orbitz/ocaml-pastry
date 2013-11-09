open Core.Std

let mapping = [| '0'; '1'; '2'; '3'
	      ;  '4'; '5'; '6'; '7'
	      ;  '8'; '9'; 'A'; 'B'
	      ;  'C'; 'D'; 'E'; 'F'
	      |]

let index arr v =
  let v = Char.uppercase v in
  let rec index' = function
    | n when n < Array.length arr -> begin
      if arr.(n) = v then
	Some n
      else
	index' (n + 1)
    end
    | _ ->
      None
  in
  index' 0

let low_bits  = 0xf

let encode s =
  let hex = String.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    hex.[2 * i]     <- mapping.((Char.to_int s.[i] lsr 4) land low_bits);
    hex.[2 * i + 1] <- mapping.(Char.to_int s.[i] land low_bits)
  done;
  hex

let decode hex =
  let s = String.create (String.length hex / 2) in
  let rec decode' = function
    | i when i < String.length s -> begin
      let high = index mapping hex.[2 * i] in
      let low  = index mapping hex.[2 * i + 1] in
      match (high, low) with
	| (Some h, Some l) -> begin
	  s.[i] <- Char.of_int_exn ((h lsl 4) lor l);
	  decode' (i + 1)
	end
	| _ ->
	  None
    end
    | _ ->
      Some s
  in
  decode' 0
