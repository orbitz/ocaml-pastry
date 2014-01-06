open Async.Std

module type IO = sig
  type t

  module Endpoint : sig
    type t
    val equal     : t -> t -> bool
    val to_string : t -> string
  end

  (* Broadcase a desire to join the cluster *)
  val announce :
    t ->
    Endpoint.t ->
    me:Endpoint.t Node.t ->
    (Endpoint.t Msg.Announce_resp.t, unit) Deferred.Result.t

  (* Have gotten an announce message, now to forward to next *)
  val announce_forward :
    t ->
    Endpoint.t ->
    Endpoint.t Msg.Announce.t ->
    (unit, unit) Deferred.Result.t

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
