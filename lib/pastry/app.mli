open Core.Std
open Async.Std

module type APP = sig
  type t

  val deliver  : t -> Message.t -> unit Deferred.t
  val forward  : t -> Message.t -> Node.t -> Message.t option Deferred.t
  val new_leaf : t -> Leaf_set.t -> unit Deferred.t
end

module type IO = sig
  type t

  val join  : t -> me:Node.t -> string -> State.t Deferred.t
  val read  : t -> Event.t Deferred.t
  val write : t -> Message.t -> (unit, Write_error.t) Result.t Deferred.t
end

module Make : functor (App : APP) -> functor (Io : IO) -> sig
  type t

  type init_args = { log      : string -> unit
		   ; node_id  : unit -> Node.t
		   ; initial  : string list
		   ; distance : Node.t -> int
		   ; b        : int
		   ; l        : int
		   ; app      : App.t
		   ; io       : Io.t
		   }

  val start : init_args -> t Deferred.t
  val stop  : t -> unit Deferred.t

  val route : t -> Message.t -> unit Deferred.t
end

