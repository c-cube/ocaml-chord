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

(** {6 Key/value store} *)

(** FIXME: no replication of key/values is done for now. *)

module type S = sig
  module DHT : Chord.S

  type id = DHT.ID.t
  type key = id
  type value = string

  val setup : ?gc:int -> DHT.t -> unit
    (** Create a key/value store that uses this DHT. The optional [gc]
        parameter is used to remove key/value pairs that have not been
        accessed for [gc] seconds. By default, key/value pairs are kept
        forever. *)

  val get : DHT.t -> key -> value option Lwt.t
    (** Get the value associated to this key, if it can be found *)

  val store : DHT.t -> key -> value -> bool Lwt.t
    (** Store the given [key -> value] in the DHT *)

  val iter : DHT.t -> (key -> value -> unit) -> unit
    (** Key/value pairs stored in the given store *)

  val on_timeout : DHT.t -> (key * value) Signal.t

  val on_store : DHT.t -> (key * value) Signal.t
end

module Make(DHT : Chord.S) : S with module DHT = DHT
