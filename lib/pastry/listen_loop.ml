open Core.Std
open Async.Std

type ('m, 'e) sender = 'm Msg.All.t -> ('m Msg.All.t, 'e) Deferred.Result.t
type ('m, 'e) listener = unit -> ('m Msg.All.t, 'e) Deferred.Result.t

let rec loop sender listener =
  listener () >>= function
    | Ok msg -> begin
      sender msg >>= fun _ ->
      loop sender listener
    end
    | Error _ ->
      Deferred.return ()

let run sender listener =
  ignore (loop sender listener)

