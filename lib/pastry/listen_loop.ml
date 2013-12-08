open Core.Std
open Async.Std

type 'a sender   = 'a Msg.All.t -> unit Deferred.t
type 'a listener = unit -> ('a Msg.All.t, unit) Deferred.Result.t

let rec loop sender listener =
  listener () >>= function
    | Ok msg -> begin
      sender msg >>= fun () ->
      loop sender listener
    end
    | Error () ->
      Deferred.return ()

let run sender listener =
  ignore (loop sender listener)

