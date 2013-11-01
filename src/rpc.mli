
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

(** {6 RPC} *)

(** This provides a lightweight RPC mechanism on top of a {!NET}
    implementation and B-encoded messages. *)

module type S = sig
  module Net : Net.S

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

module Make(Net : Net.S) : S with module Net = Net
