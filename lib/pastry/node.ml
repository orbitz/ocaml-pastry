type 'a t = { a : 'a
	    ; k : Key.t
	    ; d : int
	    }

let create ~distance ~k a =
  { d = distance; k; a }

let distance t = t.d

let key t = t.k

let of_t t = t.a

