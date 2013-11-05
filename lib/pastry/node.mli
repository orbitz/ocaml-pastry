type 'a t

val create :
  distance:int ->
  k:Key.t ->
  'a ->
  'a t

val distance : 'a t -> int
val key      : 'a t -> Key.t
val of_t     : 'a t -> 'a
