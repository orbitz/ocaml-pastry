type 'a t

val create :
  distance:int ->
  k:Key.t ->
  'a ->
  'a t

val distance     : 'a t  -> int
val key          : 'a t  -> Key.t
val closest      : Key.t -> ('a t * 'a t) -> 'a t
val find_closest : Key.t -> 'a t list     -> 'a t option
val of_t         : 'a t  -> 'a
