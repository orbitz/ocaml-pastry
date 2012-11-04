(*
 * Represents a timestamp as two integers (seconds and subseconds) so it can
 * be converted to and from something on the I/O layer.  This represents
 * up to milliseconds
 *)
open Core.Std

type t

val now          : unit -> t
val seconds      : t -> Int64.t
val milliseconds : t -> int
val compare      : t -> t -> int
