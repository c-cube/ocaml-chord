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

(** {1 Broadcasting} *)

(** We implement the algorithm from the paper
    "A partition-based Broadcast algorithm over DHT"
    to provide a broadcast primitive over a DHT. *)

(* TODO: also broadcast to a few successors, with a table of received
   broadcasted messages to avoid duplication, so that broadcasting
   resists a few node failures? *)

module type S = sig
  module Dht : Chord.S

  val start_broadcast : ?log:bool -> Dht.t -> unit
    (** Enable broadcasting on this DHT instance *)

  val enable_log : ?on:Lwt_io.output_channel -> Dht.t -> unit
    (** Enable log messages *)

  val broadcast : Dht.t -> Bencode.t -> unit
    (** Broadcast the value to every other node in the DHT *)

  val on_broadcast : Dht.t -> (Dht.ID.t * Bencode.t) Signal.t
    (** Received broadcasted values, with the ID of
        the node that started the broadcast *)

  val sent : Dht.t -> Bencode.t Signal.t
    (** Messages broadcasted from this very node *)

  val recv : Dht.t -> (Dht.ID.t * Bencode.t) Lwt.t
    (** Wait for the next broadcasted message *)
end

module Make(Dht : Chord.S) : S with module Dht = Dht
