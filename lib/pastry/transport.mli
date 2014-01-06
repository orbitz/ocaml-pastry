open Async.Std

module type IO = sig
  type t

  module Endpoint : sig
    type t
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

  val announce :
    t ->
    Endpoint.t ->
    announcer:Endpoint.t Node.t ->
    routers:Endpoint.t Router.t list ->
    (Endpoint.t Msg.Announce_resp.t, unit) Deferred.Result.t

  val send_state :
    t ->
    Endpoint.t ->
    Endpoint.t Router.t ->
    (unit, unit) Deferred.Result.t

  val send :
    t ->
    Endpoint.t ->
    Msg.Payload.t ->
    (unit, unit) Deferred.Result.t

  val ping :
    t ->
    Endpoint.t ->
    (unit, unit) Deferred.Result.t

  val distance :
    t ->
    Endpoint.t ->
    (int, unit) Deferred.Result.t

  val listen :
    t ->
    (Endpoint.t Msg.All.t, unit) Deferred.Result.t

  val close :
    t ->
    unit Deferred.t

end
