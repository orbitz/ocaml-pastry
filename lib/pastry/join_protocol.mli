open Async.Std

type 'a announce   = unit -> ('a Msg.Announce_resp.t, unit) Deferred.Result.t
type 'a send_state = 'a -> 'a Router.t -> (unit, unit) Deferred.Result.t

val join : 'a announce -> 'a send_state -> 'a Router.t -> ('a Router.t, unit) Deferred.Result.t

