open Async.Std

type ('e, 'err) sender = 'e Msg.All.t -> ('e Msg.All.t, 'err) Deferred.Result.t
type ('e, 'err) listener = unit -> ('e Msg.All.t, 'err) Deferred.Result.t

val run : ('e, 'err1) sender -> ('e, 'err2) listener -> unit
