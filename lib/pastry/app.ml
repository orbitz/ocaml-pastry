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

module Make = functor (App : APP) -> functor (Io : Transport.IO) -> struct

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
  module Join = struct
    let measure_distances io nodes =
      Deferred.List.map
	~f:(fun n ->
	  let open Deferred.Result in
	      Io.distance io (Node.of_t n) >>= fun distance ->
	      Deferred.return (Ok (Node.set_distance distance n)))
	nodes
      >>= fun res ->
      match Result.all res with
	| Ok nodes -> Deferred.return (Ok nodes)
	| Error () -> Deferred.return (Error ())

    let update_router nodes_with_distance router =
      List.fold_left
	~f:(fun r node -> Router.update ~node r)
	~init:router
	nodes_with_distance

    let notify_peers io router =
      let nodes = Router.nodes router in
      Deferred.List.map
	~f:(fun n ->
	  Io.send_state io (Node.of_t n) router)
	nodes
      >>= fun res ->
      match Result.all res with
	| Ok _     -> Deferred.return (Ok router)
	| Error () -> Deferred.return (Error ())

    let connect_to_cluster io endpoint router =
      let open Deferred.Result in
      Io.announce io endpoint ~announcer:(Router.me router) ~routers:[] >>= fun resp ->
      let nodes = Join_protocol.router_nodes ~me:(Router.me router) resp in
      measure_distances io nodes >>= fun nodes_with_distance ->
      let router' = update_router nodes_with_distance router in
      notify_peers io router'

    (*
     * This is the entry point for this module which will connect
     * to a cluster if an initial node is provided
     *)
    let maybe_connect io node router =
      match node with
	| Some endpoint ->
	  connect_to_cluster io endpoint router
	| None ->
	  Deferred.return (Ok router)
  end


  let handle_incoming state = function
    | Msg.All.Announce annc ->
      failwith "nyi"
    | Msg.All.Node_state nstate ->
      failwith "nyi"
    | _ ->
      failwith "nyi"


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
    Join.maybe_connect
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
      let module S = State in
      let key = payload.Msg.Payload.key in
      match Router.route ~k:key state.S.router with
	| me when Key.compare (Node.key me) (Node.key (Router.me state.S.router)) = 0 ->
	  failwith "nyi"
	| next_route -> begin
	  Io.send state.State.io (Node.of_t next_route) payload >>= function
	    | Ok () ->
	      Deferred.return (Gen_server.Response.Ok state)
	    | Error () ->
	      failwith "nyi"
	end
    end
    | Message.Incoming msg -> begin
      handle_incoming state msg >>= function
	| Ok state' ->
	  Deferred.return (Resp.Ok state')
	| Error () ->
	  failwith "nyi"
    end

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

