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

module type NET = sig
  module Address : sig
    type t
      (** A network address (IP:Port, typically) *)

    val encode : t -> Bencode.t
      (** Serialize the address *)

    val decode : Bencode.t -> t
      (** May raise {! Invalid_argument} *)
  end

  val send : Address.t -> Bencode.t -> unit
    (** Send a string to an address *)
end

(** {2 Configuration} *)

module type CONFIG = sig
  val redundancy : int
    (** Number of nodes to return after a lookup (the number of successors
        of the query ID). Must be >= 1 *)

  val stabilize_frequency : int
    (** Frequency at whAich stabilization of immediate neighbors is performed,
        in seconds. This should be quite frequent, and more frequent
        than {! finger_frequency} *)

  val finger_frequency: int
    (** Frequency at which Chord fingers are refreshed *)

  val timeout : float
    (** Timeout for message replies *)
end

module ConfigDefault : CONFIG
  (** Default parameters *)

(** {2 DHT state} *)

(** The DHT is a 120-bits addressing Chord network. This module provides a
    functor that is message-agnostic, and only works on events.
    see {{: http://en.wikipedia.org/wiki/Chord_%28peer-to-peer%29 } wikipedia}.
    for a description of Chord. *)

(** Signature of the DHT *)
module type S = sig
  module Net : NET
  module Config : CONFIG

  type id = string
    (** A string that uniquely identifies a node on the DHT *)

  type address = Net.Address.t

  type t
    (** Instance of the DHT. This contains the state (routing table,
        fingers, etc.) of a node of the DHT. *)

  type node
   (** The representation of a node of the DHT. It relates an ID (a hash
        uniquely identifying the remote node in the DHT) to a network address
        and a node payload (private key, owner metadata, etc.) *)

  val random_id : unit -> id
    (** A fresh, unique ID usable on the network *)

  val create : ?id:id -> ?payload:string -> address -> t
    (** New DHT, using the given network node. If no ID is provided,
        a new random one is used. The address of the local node
        must be provided.
        [payload] is an optional string that is attached to the newly
        created node. *)

  val local : t -> node
    (** Node that represents this very DHT node *)

  val id : node -> id
    (** ID of the given DHT node *)

  val address : node -> address
    (** Address of the node *)

  val payload : node -> string
    (** Payload of a node *)

  val connect : t -> address -> id option Lwt.t
    (** Try to connect to the remote note, returns the ID of the
        node on success. *)

  val find_node : t -> id -> node option Lwt.t
    (** Returns the successor node of the given ID. It may fail, in
        which case [None] is returned. *)

  val send : t -> id -> Bencode.t -> unit
    (** Send the given message to the {! Config.redundancy} successors of
        the given ID *)

  val receive : t -> Bencode.t -> unit
    (** Have the DHT process this incoming message *)

  val tick : t -> unit
    (** Tick, should be called regularly (frequently enough, say,
        every second). It is used to check timeouts. *)

  (** {2 Register to events} *)

  val on_message : t -> (Bencode.t -> unit) -> unit
    (** Subscribe to generic messages sent to this node. It is the
        receiving-side counterpart to {!send}. *)

  type change_event =
    | Join of node
    | Part of node

  val on_change : t -> (change_event -> unit) -> unit
    (** Register to {!change_event}s. Not all join/parts are known to the
        local node, but it is still an interesting information (especially
        about immediate redundancy). *)
end

module Make(Net : NET)(Config : CONFIG)
  : S with module Net = Net and module Config = Config
