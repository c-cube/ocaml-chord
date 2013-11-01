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

(** {6 B-encoding} *)

(** This implements encoding and decoding using the {i B-encode} format.
    See {{: http://en.wikipedia.org/wiki/Bencode} wikipedia} for more details
    *)

module SMap : Map.S with type key = string

type t =
  | I of int
  | S of string
  | L of t list
  | D of t SMap.t

val eq : t -> t -> bool
val hash : t -> int

val dict_of_list : (string * t) list -> t

(** {2 Serialization (encoding)} *)

val size : t -> int
  (** Size needed for serialization *)

val write_in_string : t -> string -> int -> unit
  (** [write_in_string v buf o] writes the value [v] in  the string,
      starting at offset [o]. The portion of the string starting from [o]
      must be big enough (ie >= [size v]) *)

val to_buf : Buffer.t -> t -> unit
val to_string : t -> string
val to_chan : out_channel -> t -> unit
val fmt : Format.formatter -> t -> unit

val pretty : Format.formatter -> t -> unit
  (** Print the tree itself, not its encoding *)

val pretty_to_str : t -> string
  (** Print the tree into a string *)

(** {2 Deserialization (decoding)} *)

(** Deserialization is based on the {! decoder} type. Parsing can be
    incremental, in which case the input is provided chunk by chunk and
    the decoder contains the parsing state. Once a B-encoded value
    has been parsed, other values can still be read.
    
    This implementation does accept leading zeros, because it simplifies
    the code. *)

type decoder
  (** Decoding state *)

val mk_decoder : unit -> decoder
  (** Create a new decoder *)

type parse_result =
  | ParseOk of t
  | ParseError of string
  | ParsePartial

val parse : decoder -> string -> int -> int -> parse_result
  (** [parse dec s i len] uses the partial state stored in [dec] and
      the substring of [s] starting at index [i] with length [len].
      It can return an error, a value or just [ParsePartial] if
      more input is needed *)

val reset : decoder -> unit
  (** Reset the decoder to its pristine state, ready to parse something
      different. Before that, {! rest} and {! rest_size} can be used
      to recover the part of the input that has not been consumed yet. *)

val state : decoder -> parse_result
  (** Current state of the decoder *)

val rest : decoder -> string
  (** What remains after parsing (the additional, unused input) *)

val rest_size : decoder -> int
  (** Length of [rest d]. 0 indicates that the whole input has been consumed. *)

val parse_string : string -> parse_result
  (** Parse a full value from this string. *)

val of_string : string -> t
  (** Parse the string. @raise Invalid_argument if it fails to parse. *)
