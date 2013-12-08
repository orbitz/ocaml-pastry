module Announce : sig
  type 'a t = 'a Node.t
end

module Announce_resp : sig
  type 'a t = ('a Node.t * 'a Router.t) list
end

module Node_state : sig
  type 'a t = 'a Router.t
end

module Payload : sig
  type t = { key     : Key.t
	   ; payload : string
	   }
end


module All : sig
  type 'a t =
    | Announce      of 'a Announce.t
    | Announce_resp of 'a Announce_resp.t
    | Node_state    of 'a Node_state.t
    | Payload       of Payload.t
end
