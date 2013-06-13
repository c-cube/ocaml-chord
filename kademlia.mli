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

(** {6 Kademlia DHT} *)

(** {2 Communications} *)

(** The interface offered by the network *)
module type NET = sig
  type connection_cache
    (** Cache for re_using connections *)

  val mk_cache : unit -> connection_cache

  type message
    (** Type of messages that can be sent *)

  type address
    (** Identifier of a (possibly remote) node on the network. *)

  val send : ?cache:connection_cache -> address -> message -> unit
    (** Send a message *)

  type recv_event =
    | Receive of address * message
    | Ping of address

  val receive : ?cache:connection_cache -> address -> recv_event Lwt.t
    (** Receive a message *)
end

(** {2 DHT} *)

(** configuration of the DHT *)
module type CONFIG = sig
  val parallel_lookup : int
    (** Number of hash lookups to perform in parallel *)

  val k : int
    (** Number of nodes in one bucket *)

  val n : int
    (** Number of bits for hashes/IDs *)
end

(** Signature of the DHT *)
module type S = sig
  module Net : NET
  module Config : CONFIG

  type address = Net.address
  type 

  type t
    (** Instance of the DHT *)

  type id = string
    (** An ID is a resource locator. It consists in a [2^Net.n]-bits SHA1 *)

  type node = {
    node_id : id;
    node_address : address;
  }

  val create : Net.node -> t
    (** New DHT, using the given network node *)

  val connect : t -> Net.address -> id option Lwt.t
    (** Try to connect to the remote note, returns the ID of the
        node on success. *)

  val find_node : t -> id -> id list Lwt.t
    (** Returns the list of the {! Net.k} nodes that are the closest to
        the given ID *)

end

module Make(Config : CONFIG)(Net : NET) :
  S with module Net = Net and module Config = Config

(** {2 Key/Value storage} *)
