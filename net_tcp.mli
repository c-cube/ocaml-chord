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

(** {1 Network implementation using TCP sockets} *)

include Net.S with type Address.t = Unix.inet_addr * int
  (** Adresses are just a handle to some in-memory object *)

val mk_addr : string -> int -> Address.t
val mk_local_addr : int -> Address.t

val mk_by_name : string -> int -> Address.t option Lwt.t
  (** DNS lookup to find an IP, and make an adress out of
      it and the given port number *)

val create : ?size:int -> ?log:bool -> ?port:int -> unit -> t option
  (** Create a new network node on the given port, if provided
      (a random port otherwise). May return None if it is impossible
      to create a socket on the given port.
      [size] is the max number of parallel connections. *)

val port : t -> int
  (** Port used by the network node *)

val stop : t -> unit
  (** Stop this node (closes the TCP sockets) *)

val sent : t -> (Address.t * Bencode.t) Signal.t
  (** Sent messages *)

val enable_log : ?on:out_channel -> t -> unit
  (** Enable logging of events on this network node *)

val fmt : Format.formatter -> Address.t -> unit
val to_string : Address.t -> string
