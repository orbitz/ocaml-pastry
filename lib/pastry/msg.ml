module Announce = struct
  type 'a t = 'a Node.t
end

module Announce_resp = struct
  type 'a t = 'a Router.t list
end

module Node_state = struct
  type 'a t = 'a Router.t
end

module Payload = struct
  type t = { key     : Key.t
           ; payload : string
           }
end


module All = struct
  type 'a t =
    | Announce      of 'a Announce.t
    | Announce_resp of 'a Announce_resp.t
    | Node_state    of 'a Node_state.t
    | Payload       of Payload.t
end
