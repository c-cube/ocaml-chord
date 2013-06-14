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
  type address
    (** A network address (IP:Port, typically) *)

  val address_of_string : string -> address
    (** May raise {! Invalid_argument} *)

  val string_of_address : address -> string

  val send : address -> string -> unit
    (** Send a string to an address *)

  val receive : (unit -> string) -> unit
    (** Subscribe to incoming messages *)

  val wait : timeout:int -> unit Lwt.t
    (** Wait for the given amount of seconds before returning *)

  val periodically : freq:int -> (unit -> unit) -> unit
    (** Subscribe to a periodic event. The duration [freq] is in seconds. *)
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
end

module ConfigDefault : CONFIG = struct
  let redundancy = 5

  let stabilize_frequency = 30

  let finger_frequency = 120
end

(** {2 DHT state} *)

(** The DHT is a 120-bits addressing Chord network. This module provides a
    functor that is message-agnostic, and only works on events.

    @see {{: http://en.wikipedia.org/wiki/Chord_%28peer-to-peer%29 } wikipedia}.
    for a description of Chord. *)

(** Signature of the DHT *)
module type S = sig
  module Net : NET
  module Config : CONFIG

  val n : int
    (** Number of bits in the ID space *)

  type id = string
    (** A {!n}-bits string *)

  type address = Net.address

  type t
    (** Instance of the DHT. This contains the state (routing table,
        fingers, etc.) of a node of the DHT. *)

  type node = {
    node_id : id;
    node_address : address;
    node_payload : string;    (** Additional data for the node *)
  } (** The representation of a remote node. It relates an ID (a hash
        uniquely identifying the remote node in the DHT) to a network address
        and a node payload (private key, owner metadata, etc.) *)

  val random_id : unit -> id
    (** A fresh, unique ID usable on the network *)

  val create : ?id:id -> ?payload:string -> unit -> t
    (** New DHT, using the given network node. If no ID is provided,
        a new random one is used.
        [payload] is an optional string that is attached to the newly
        created node. *)

  val id : t -> id
    (** ID of the local DHT node *)

  val connect : t -> address -> id option Lwt.t
    (** Try to connect to the remote note, returns the ID of the
        node on success. *)

  val find_node : t -> id -> id list Lwt.t
    (** Returns the list of the {! Config.redundancy} nodes that are the closest to
        the given ID *)

  val send : t -> id -> string -> unit
    (** Send the given message to the {! Config.redundancy} successors of
        the given ID *)

  val on_message : t -> (string -> unit) -> unit
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

module Make(Net : NET)(Config : CONFIG) = struct
  module Net = Net
  module Config = Config

  let n = 160
    (** Number of bits in the ID space *)

  type id = string
    (** A {!n}-bits string *)

  type address = Net.address

  type t = {
    id : id; (* my id *)
    mutable predecessor : node option;
    mutable successors : node list;  (* a list of the {! Config.redundancy} successors *)
    fingers : fingers
  } (** The local node *)

  and fingers = node list array
    (** The table of fingers *)

  and node = {
    node_id : id;
    node_address : address;
    node_payload : string;    (** Additional data for the node *)
  } (** The representation of a remote node. It relates an ID (a hash
        uniquely identifying the remote node in the DHT) to a network address
        and a node payload (private key, owner metadata, etc.) *)

  and change_event =
    | Join of node
    | Part of node

  let random_id () =
    let s = String.create (n / 8) in
    for i = 0 to (n/8) - 1 do
      s.[i] <- (Char.chr (Random.int 256));
    done;
    s

  let create ?(id=random_id()) ?(payload="") () =
    let dht = {
      id;
      predecessor=None;
      successors=[];
      fingers = Array.create n [];
    } in
    (* TODO subscribe to network events, etc. *)
    dht

  (* Get the ID of the DHT. Returns a copy, to be sure that
    it's not modified *)
  let id dht =
    String.copy dht.id

  let connect dht addr =
    failwith "connect: not implemented"

  let find_node dht id =
    failwith "find_node: not implemented"

  let send dht id msg =
    failwith "send: not implemented"

  let on_message id k =
    failwith "on_message: not implemented"

  let on_change dht k =
    failwith "on_change: not implemented"
end
