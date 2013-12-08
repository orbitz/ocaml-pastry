open Async.Std

type 'a sender = 'a Msg.All.t -> unit Deferred.t
type 'a listener = unit -> ('a Msg.All.t, unit) Deferred.Result.t

val run : 'a sender -> 'a listener -> unit
