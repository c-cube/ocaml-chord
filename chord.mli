(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {6 Chord DHT} *)

(** {2 Network} *)

(** The network module is an abstraction over communication with other nodes.
    It allows to designates other entities via {b addresses} (for instance,
    process IDs, or IP+port addresses), and to send and receive messages
    as B-encoded data. A primitive to wait is also provided.

    A typical implementation may use TCP connections to send and receive
    messages. *)

module type NET = sig
  module Address : sig
    type t
      (** A network address (IP:Port, typically) *)

    val encode : t -> Bencode.t
      (** Serialize the address *)

    val decode : Bencode.t -> t
      (** May raise {! Invalid_argument} *)

    val eq : t -> t -> bool
  end

  type t
    (** A node on the network *)

  val send : t -> Address.t -> Bencode.t -> unit
    (** Send a string to an address *)

  type event =
    | Receive of Address.t * Bencode.t   (* received message *)
    | Stop  (* stop the DHT *)

  val events : t -> event Signal.t
    (** Signal transmitting events that occur on the network *)

  val call_in : float -> (unit -> unit) -> unit
    (** Call the function in the given amount of seconds *)
end

(** {2 RPC} *)

(** This provides a lightweight RPC mechanism on top of a {!NET}
    implementation and B-encoded messages. *)

module type RPC = sig
  module Net : NET

  type address = Net.Address.t

  type t
    (** A RPC system *)

  type reply_tag
    (** A tag used to reply to messages *)

  val create : ?frequency:float -> Net.t -> t
    (** Create an instance of the RPC system, which can send and receive
        remote function calls using the [Net.t] instance.
        [frequency] is the frequency, in seconds, at which the
        RPC system checks whether some replies timed out. *)

  val notify : t -> address -> Bencode.t -> unit
    (** Send a message without expecting a reply *)

  val send : t -> ?timeout:float -> address -> Bencode.t -> Bencode.t option Lwt.t
    (** Send a message, expecting a reply *)

  val reply : t -> reply_tag -> Bencode.t -> unit
    (** Reply to the message whose tag is given *)

  val received : t -> (address * reply_tag option * Bencode.t) Signal.t
    (** Signal incoming messages. The signal transmits the sender's
        address, a reply tag (in case the sender expected a reply)
        and the message itself *)

  val stop : t -> unit
    (** Disable all threads and active processes *)
end

module MakeRPC(Net : NET) : RPC with module Net = Net

(** {2 Configuration} *)

(** This module contains values that parametrize the DHT's behavior. *)

module type CONFIG = sig
  val dimension : int
    (** Logarithm (base 2) of the size of the ID space. Default is 160. *)

  val redundancy : int
    (** Number of nodes to return after a lookup (the number of successors
        of the query ID). Must be >= 1 *)

  val stabilize_frequency : float
    (** Frequency at whAich stabilization of immediate neighbors is performed,
        in seconds. This should be quite frequent, and more frequent
        than {! finger_frequency} *)

  val finger_frequency: float
    (** Frequency at which Chord fingers are refreshed *)

  val node_timeout : float
    (** After this amount of time (in seconds), a node that does not reply
        is considered dead *)

  val timeout : float
    (** Timeout for message replies (in seconds) *)
end

module ConfigDefault : CONFIG
  (** Default parameters *)

(** {2 DHT} *)

(** The DHT is a Chord network. It uses {!NET} for communications, and
    {!CONFIG} for its parameters.

    see {{: http://en.wikipedia.org/wiki/Chord_%28peer-to-peer%29 } wikipedia}.
    for a description of Chord. *)

(** Signature of the DHT *)
module type S = sig
  module Net : NET
  module Config : CONFIG
  module Rpc : RPC with module Net = Net

  (** Unique identifier for a node *)
  module ID : sig
    type t

    val of_string : string -> t
      (** Parses an ID from a string, or @raise Invalid_argument *)

    val to_string : t -> string

    val eq : t -> t -> bool

    val hash : t -> int
  end

  type address = Net.Address.t

  type t
    (** Instance of the DHT. This contains the state (routing table,
        fingers, etc.) of a node of the DHT. *)

  type node
   (** The representation of a node of the DHT. It relates an ID (a hash
        uniquely identifying the remote node in the DHT) to a network address
        and a node payload (private key, owner metadata, etc.) *)

  val random_id : unit -> ID.t
    (** A fresh, unique ID usable on the network *)

  val create : ?log:bool -> ?id:ID.t -> ?payload:string -> Net.t -> t
    (** New DHT, using the given network node. If no ID is provided,
        a new random one is used.
        [payload] is an optional string that is attached to the newly
        created node. *)

  val local : t -> node
    (** Node that represents this very DHT node *)

  val rpc : t -> Rpc.t
    (** RPC instance *)

  val id : node -> ID.t
    (** ID of the given DHT node *)

  val addresses : node -> address list
    (** Address(es) the node can be contacted with. *)

  val payload : node -> string
    (** Payload of a node *)

  val connect : t -> address -> node option Lwt.t
    (** Try to connect to the remote note, returns the remote node
        on success. *)

  val find_node : t -> ID.t -> node option Lwt.t
    (** Returns the successor node of the given ID. It may fail, in
        which case [None] is returned. *)

  val successor : t -> node
    (** Current successor of this node *)

  val successors : t -> int -> node list
    (** Find the [k] successors of this node *)

  (** {2 Register to events} *)

  type change_event =
    | NewNode of node
    | Timeout of node

  val changes : t -> change_event Signal.t
    (** Changes in the network. Not all join/parts are known to the local node,
        but it is still an interesting information (especially about immediate
        redundancy). *)

  (** {2 Overlay network} *)

  module OverlayNet : NET with type Address.t = ID.t and type t = t
    (** See the DHT as a networking device *)

  module AsRPC : RPC with module Net = OverlayNet
    (** Remote procedure calls on the overlay network *)

  (** {2 Misc} *)

  val enable_log : ?on:out_channel -> t -> unit
    (** Print events related to the DHT on the given channel *)

  val wait : t -> unit Lwt.t
    (** Wait for the DHT to stop *)

  val stop : t -> unit
    (** Stop the DHT *)

  val stopped : t -> bool
    (** Is the DHT already stopped? *)

  val log : t -> ('a, Buffer.t, unit, unit) format4 -> 'a
    (** Log a message *)
end

module Make(Net : NET)(Config : CONFIG)
  : S with module Net = Net and module Config = Config
