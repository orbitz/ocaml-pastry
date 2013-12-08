open Core.Std
open Async.Std

type 'a announce   = unit -> ('a Msg.Announce_resp.t, unit) Deferred.Result.t
type 'a send_state = 'a -> 'a Router.t -> (unit, unit) Deferred.Result.t

let join announce send_state router =
  failwith "nyi"


