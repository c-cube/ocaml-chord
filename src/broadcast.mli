
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

(** {1 Simple broadcast} *)

(** This module implements a simple networking protocol based on naive
    broadcasting. Nodes simply broadcast the messages to their
    neighbors, and keep a table of recently received messages to
    eliminate duplicates. *)

module type S = sig
  module N : Net.S

  type t
    (** Broadcasting algorithm state. This accounts for one node
        in the broadcasting network. *)

  val create : ?cache_timeout:float -> N.t -> t
    (** Create a new broadcasting device *)

  val broadcast : t -> Bencode.t -> unit
    (** Broadcast a message *)

  val connect : t -> N.Address.t -> bool Lwt.t
    (** Connect to another node. Returns true iff it succeeds. *)

  val neighbors : t -> N.Address.t list
    (** Current list of neighbors *)

  type event =
    | Receive of Bencode.t

  val events : t -> event Signal.t
    (** messages broadcasted by others *)

  val recv : t -> event Lwt.t
    (** next event *)
end

module Make(N : Net.S) : S with module N = N
