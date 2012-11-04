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
		   ; distance : Node.t -> int option Deferred.t
		   ; b        : int
		   ; l        : int
		   ; app      : App.t
		   ; io       : Io.t
		   }

  module State = struct
    type t = { log       : string -> unit
	     ; node_id   : Node.t
	     ; timestamp : Core.Time.t
	     ; distance  : Node.t -> int option Deferred.t
	     ; b         : int
	     ; app       : App.t
	     ; io        : Io.t
	     ; rt        : Routing_table.t
	     ; ns        : Neighborhood_set.t
	     ; ls        : Leaf_set.t
	     ; mq        : Msg.t Tail.t
	     }
  end

  type t = Msg.t Tail.t

  (*
   * New connection as viewed from an already established node:
   * - Receives a Node_join message
   * - Routes the Node_join message like any other message
   * - When the routing comes back successful sends state to
   *   the joined node
   * - Upon success of sending the state, success is returned
   *   to sender of Node_join message
   *
   * In this way a long chain all the way to the destination
   * node is formed and the joining node only sees success if
   * the entire path succeeds.  If a node in between fails the
   * routing mechanism will retry and if that fails the error
   * will propogate all the way back to the joining node.
   * A joining node may receive more states than requests due
   * to node failures and retries along the way.
   *
   * New connection from a joining node:
   * - Starts event loop as if it is it's own cluster
   * - Loops over `initial` list sending Node_join message
   * - If Node_join message send fails, try next in `initial`
   * - If all fail, stop the event loop then exit with failure
   * - If Node_join succeeds, the event loop will receive state
   *   updates from the appropriate nodes and incorporates it
   *   in its state
   *)
  let connect t =
    Deferred.return (Result.Error ())


  let rec io_msg_loop io mq =
    Io.read io >>> fun event ->
    if Msg.send mq (Msg.Io_event event) then
      io_msg_loop io mq


  let handle_route s (m, r) =
    Deferred.return s

  let handle_io_msg s = function
    | Event.Node_join node -> ()
    | Event.Node_joined node -> ()
    | Event.Node_state (r, ns, ls) -> ()
    | Event.Node_part node -> ()
    | Event.Message m -> ()

  let handle_msg s = function
    | Route msg    -> handle_route s msg
    | Stop         -> begin Tail.close_if_open s.State.mq; Deferred.return s end
    | Io_event msg -> handle_io_msg s msg

  let rec loop s =
    if not (Tail.is_closed s.State.mq) then
      let stream = Tail.collect s.State.mq in
      Stream.fold'
	~f:handle_msg
	~init:t
	stream >>= loop
    else
      Deferred.return ()

  (*
   * API
   *)
  let stop t =
    ignore (Msg.send t Msg.Stop);
    Deferred.return ()

  let start ia =
    let module S = State in
    let node_id  = ia.node_id () in
    let rt       = Routing_table.make ~me:node_id ~b:ia.b ia.distance in
    let ns       = Neighborhood_set.make ~me:node_id ia.l in
    let ls       = Leaf_set.make ~me:node_id ia.l in
    let s        = { S.log       = ia.log
		   ;   node_id   = node_id
		   ;   timestamp = Core.Time.now ()
		   ;   distance  = ia.distance
		   ;   b         = ia.b
		   ;   app       = ia.app
		   ;   io        = ia.io
		   ;   rt        = rt
		   ;   ns        = ns
		   ;   ls        = ls
		   }
    in
    whenever (loop s);
    connect t >>= function
      | Result.Ok s' -> begin
	Deferred.return (Result.Ok s'.State.mq)
      end
      | Result.Error err -> begin
	stop s >>= fun () ->
	Deferred.return (Result.Error err)
      end

  let route t m =
    let r = Ivar.create () in
    ignore (Msg.send t (Msg.route m, r));
    Ivar.read r
end

