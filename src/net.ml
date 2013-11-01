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

(** {1 Network abstraction} *)

(** The network module is an abstraction over communication with other nodes.
    It allows to designates other entities via {b addresses} (for instance,
    process IDs, or IP+port addresses), and to send and receive messages
    as B-encoded data. A primitive to wait is also provided.

    A typical implementation may use TCP connections to send and receive
    messages. *)

module type S = sig
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
    | Stop  (* stopped receiving messages*)

  val events : t -> event Signal.t
    (** Signal transmitting events that occur on the network *)

  val call_in : float -> (unit -> unit) -> unit
    (** Call the function in the given amount of seconds *)
end
