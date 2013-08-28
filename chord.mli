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

module type NET = Net.S
module type RPC = Rpc.S

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
  module Net : Net.S
  module Config : CONFIG
  module Rpc : Rpc.S with module Net = Net

  (** Unique identifier for a node *)
  module ID : sig
    type t

    val of_string : string -> t
      (** Parses an ID from a string, or
          @raise Invalid_argument if the string is no valid ID *)

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
        and a node payload (private key, owner metadata, etc.).
        
        There is an injection from IDs to nodes, but not all IDs
        are necessarily used at a given time. The distinction
        between [t] and [node] is that [node] represents any
        participant to the DHT, whereas [t] is responsible for
        the local node (the one in this OCaml process). *)

  val random_id : unit -> ID.t
    (** A fresh, unique ID usable on the network *)

  val create : ?log:bool -> ?id:ID.t -> ?payload:string -> Net.t -> t
    (** New DHT, using the given network node. If no ID is provided,
        a new random one is used.
        [payload] is an optional string that is attached to the newly
        created node. *)

  val local : t -> node
    (** Node that represents this very DHT instance. *)

  val rpc : t -> Rpc.t
    (** RPC instance *)

  val id : node -> ID.t
    (** ID of the given DHT node *)

  val addresses : node -> address list
    (** Address(es) the node can be contacted with, to our knowledge. *)

  val payload : node -> string
    (** Payload of a node *)

  val connect : t -> address -> node option Lwt.t
    (** Try to connect to the remote node, returns a proxy to it
        on success. *)

  val find_node : t -> ID.t -> node option Lwt.t
    (** Returns the successor node of the given ID, ie the node whose ID
        is the first one to be bigger or equal to the given ID. It may fail, in
        which case [None] is returned. *)

  (** {2 Topology of the network} *)

  module Topology : sig

    val successor : t -> node
      (** Current successor of this node in the Chord *)

    val predecessor : t -> node
      (** Immediate predecessor in the Chord *)

    val successors : t -> int -> node list
      (** [successors dht k] finds the [k] successors of [dht] *)

    val finger : t -> int -> node
      (** [finger dht k] is the [k]-th finger, ie the successor of the 
          ID [(local + 2 ** k) mod 2 ** dimension] *)

    val between : ID.t -> ID.t -> ID.t -> bool
    val between_strict : ID.t -> ID.t -> ID.t -> bool
    val succ : ID.t -> ID.t
    val pred : ID.t -> ID.t
  end

  (** {2 Register to events} *)

  type change_event =
    | NewNode of node
    | Timeout of node

  val changes : t -> change_event Signal.t
    (** Changes in the network. Not all join/parts are known to the local node,
        but it is still an interesting information (especially about immediate
        redundancy). *)

  (** {2 Mixtbl store for adding features to the DHT} *)

  (** This section is to be used by modules  that want to store data
      in the local DHT instance (See for instance {! Store} or {! AsRPC}). *)

  module Mixtbl : sig
    val table : t -> string Mixtbl.t
      (** Raw access to the mixtbl *)

    val access : unit -> (string,'a) Mixtbl.injection
      (** New accessor for some type 'a *)

    val set : inj:(string,'a) Mixtbl.injection -> t -> string -> 'a -> unit
      (** Add a key/value pair *)

    val get : inj:(string,'a) Mixtbl.injection -> t -> string -> 'a
      (** Retrieve the value for this given key, or
          @raise Not_found if no value is associated to this key *)
  end

  (** {2 Overlay network} *)

  (** The two following modules allow the user to use the DHT as if it were
      a Network implementation (signature {! NET}), respectively a RPC
      messaging system. Messages can be sent to nodes by their ID, and
      even if an ID does not exist, the message will be sent to its
      successor ID (See {!find_node}). *)

  module OverlayNet : NET with type Address.t = ID.t and type t = t
    (** See the DHT as a networking device *)

  module AsRPC : RPC with module Net = OverlayNet and type t = t
    (** Remote procedure calls on the overlay network *)

  (** {2 Misc} *)

  val enable_log : ?on:out_channel -> t -> unit
    (** Print events related to the DHT on the given channel *)

  val wait : t -> unit Lwt.t
    (** Wait for the DHT to stop *)

  val stop : t -> unit
    (** Stop the DHT. It will no respond to messages nor maintain
        topological invariants anymore. *)

  val stopped : t -> bool
    (** Is the DHT already stopped? *)

  val log : t -> ('a, Buffer.t, unit, unit) format4 -> 'a
    (** Log a message to channels on which {!enable_log} has
        been called. *)
end

module Make(Net : NET)(Config : CONFIG)
  : S with module Net = Net and module Config = Config
