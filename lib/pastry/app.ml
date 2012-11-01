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

module Msg = struct
  type t =
    | Route of (Message.t * unit Ivar.t)
    | Stop
    | Io_event of Event.t

  let send mq m =
    try
      Tail.extend mq m;
      true
    with
	Failure _ ->
	  false
end


module Make = functor (App : APP) -> functor (Io : IO) -> struct
  type init_args = { log      : string -> unit
		   ; node_id  : unit -> Node.t
		   ; initial  : string list
		   ; distance : Node.t -> int
		   ; b        : int
		   ; l        : int
		   ; app      : App.t
		   ; io       : Io.t
		   }

  module State = struct
    type t = { log      : string -> unit
	     ; node_id  : Node.t
	     ; initial  : string list
	     ; distance : Node.t -> int
	     ; b        : int
	     ; l        : int
	     ; app      : App.t
	     ; io       : Io.t
	     ; rt       : Routing_table.t
	     ; ns       : Neighbourhood_set.t
	     ; ls       : Leaf_set.t
	     ; mq       : Msg.t Tail.t
	     }
  end

  type t = State.t


  let connect t =
    Deferred.return (Result.Error ())


  let rec io_msg_loop io mq =
    Io.read io >>> fun event ->
    if Msg.send mq (Msg.Io_event event) then
      io_msg_loop io mq


  let handle_route t (m, r) =
    Deferred.return t

  let handle_io_msg t = function
    | Event.Node_join node -> ()
    | Event.Node_joined node -> ()
    | Event.Node_state (r, ns, ls) -> ()
    | Event.Node_part node -> ()
    | Event.Message m -> ()

  let handle_msg t = function
    | Route msg    -> handle_route t msg
    | Stop         -> begin Tail.close_if_open t.mq; Deferred.return t end
    | Io_event msg -> handle_io_msg t msg

  let rec loop t =
    if not (Tail.is_closed t.State.mq) then
      let stream = Tail.collect t.State.mq in
      Stream.fold'
	~f:handle_msg
	~init:t
	stream >>= loop
    else
      Deferred.return ()

  let start ia =
    let module S = State in
    let node_id  = ia.node_id () in
    let t        = { S.log      = ia.log
		   ;   node_id  = node_id
		   ;   initial  = ia.initial
		   ;   distance = ia.distance
		   ;   b        = ia.b
		   ;   l        = ia.l
		   ;   app      = ia.app
		   ;   io       = ia.io
		   ;   rt       = Routing_table.make b node_id
		   ;   ns       = Neighbourhood_set.make ()
		   ;   ls       = Leaf_set.make ()
		   }
    in
    connect t >>= function
      | Result.Ok t' -> begin
	whenever (loop t');
	Deferred.return (Result.Ok t')
      end
      | Result.Error err -> begin
	Deferred.return (Result.Error err)
      end

  let stop t =
    ignore (Msg.send t.State.mq Msg.Stop);
    Deferred.return ()

  let route t m =
    let r = Ivar.create () in
    ignore (Msg.send t.State.mq (Msg.route m, r));
    Ivar.read r
end

