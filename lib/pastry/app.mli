open Core.Std
open Async.Std

module type APP = sig
  type 'a t

  val deliver :
    'a t ->
    Msg.Payload.t ->
    (unit, unit) Deferred.Result.t

  val forward :
    'a t ->
    Msg.Payload.t ->
    'a Node.t ->
    (Msg.Payload.t * 'a Node.t) option Deferred.t

  val new_leaf_set :
    'a t ->
    'a Leaf_set.t ->
    unit Deferred.t
end

module type IO = sig
  type t

  module Endpoint : sig
    type t
    val to_string : t -> string
  end

  val announce   : t -> Endpoint.t -> (Endpoint.t Msg.Announce_resp.t, unit) Deferred.Result.t
  val send_state : t -> Endpoint.t -> Endpoint.t Router.t -> (unit, unit) Deferred.Result.t
  val send       : t -> Endpoint.t -> Msg.Payload.t -> (unit, unit) Deferred.Result.t
  val ping       : t -> Endpoint.t -> (unit, unit) Deferred.Result.t
  val distance   : t -> Endpoint.t -> (int, unit) Deferred.Result.t
  val listen     : t -> (Endpoint.t Msg.All.t, unit) Deferred.Result.t
  val close      : t -> unit Deferred.t
end

module Make : functor (App : APP) -> functor (Io : IO) -> sig
  type t

  type init_args = { node_id : Io.Endpoint.t Node.t
		   ; app     : Io.Endpoint.t App.t
		   ; io      : Io.t
		   ; connect : Io.Endpoint.t option
		   }

  val start : init_args -> (t, unit) Deferred.Result.t
  val stop  : t -> unit Deferred.t
  val route : t -> Msg.Payload.t -> unit Deferred.t
end

