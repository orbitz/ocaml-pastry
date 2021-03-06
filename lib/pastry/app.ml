open Core.Std
open Async.Std

module Resp = Gen_server.Response

(* Hardcoded for now *)
let b = 4

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
    val equal     : t -> t -> bool
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


module Make = functor (App : APP) -> functor (Io : IO) -> struct

  type init_args = { node_id : Io.Endpoint.t Node.t
		   ; app     : Io.Endpoint.t App.t
		   ; io      : Io.t
		   ; connect : Io.Endpoint.t option
		   }


  module State = struct
    type t = { node_id   : Io.Endpoint.t Node.t
	     ; timestamp : Core.Time.t
	     ; app       : Io.Endpoint.t App.t
	     ; io        : Io.t
	     ; router    : Io.Endpoint.t Router.t
	     }
  end

  module Message = struct
    type t =
      | Route of Msg.Payload.t
      | Incoming of Io.Endpoint.t Msg.All.t
  end

  type t = Message.t Gen_server.t

  (*
   * Internal API
   *)
  let maybe_connect io node router =
    match node with
      | Some endpoint ->
	Deferred.return (Error ())
      | None ->
	Deferred.return (Ok router)

  let send_incoming gs m =
    let open Deferred.Result in
    Gen_server.send gs (Message.Incoming m) >>= fun _ ->
    Deferred.return (Ok m)

  (*
   * Gen_server callbacks
   *)
  let init self init_args =
    let state = { State.node_id   = init_args.node_id
		;       timestamp = Core.Time.now ()
		;       app       = init_args.app
		;       io        = init_args.io
		;       router    = Router.create ~me:init_args.node_id ~b
		}
    in
    maybe_connect
      state.State.io
      init_args.connect
      state.State.router
    >>= function
      | Ok router -> begin
	Listen_loop.run
	  (send_incoming self)
	  (fun () -> Io.listen state.State.io);
	Deferred.return (Ok { state with State.router = router })
      end
      | Error () ->
	Deferred.return (Error ())

  let handle_call _self state = function
    | Message.Route payload -> begin
      let key = payload.Msg.Payload.key in
      let next_route = Router.route ~k:key state.State.router in
      Io.send state.State.io (Node.of_t next_route) payload >>= function
	| Ok () ->
	  Deferred.return (Gen_server.Response.Ok state)
	| Error () ->
	  failwith "bad mojo"
    end
    | Message.Incoming msg ->
      Deferred.return (Resp.Ok state)

  let terminate _reason state =
    Io.close state.State.io

  (*
   * API
   *)
  let start init_args =
    let gs = { Gen_server.Server.init; handle_call; terminate } in
    Gen_server.start init_args gs

  let stop = Gen_server.stop

  let route t payload =
    Gen_server.send t (Message.Route payload) >>= function
      | Ok _    -> Deferred.return (Ok ())
      | Error _ -> Deferred.return (Error ())

end

