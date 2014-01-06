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

module Make : functor (App : APP) -> functor (Io : Transport.IO) -> sig
  type t

  type init_args = { node_id : Io.Endpoint.t Node.t
                   ; app     : Io.Endpoint.t App.t
                   ; io      : Io.t
                   ; connect : Io.Endpoint.t option
                   }

  val start : init_args -> (t, [> unit Gen_server.init_ret ]) Deferred.Result.t
  val stop  : t -> (unit, [> `Closed ]) Deferred.Result.t
  val route : t -> Msg.Payload.t -> (unit, unit) Deferred.Result.t
end

