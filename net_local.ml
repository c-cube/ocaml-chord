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

(** {1 Single process Network implementation} *)

module B = Bencode

module Address = struct
  type t = int
    (** Adresses are just a handle to some thread *)

  let encode a = B.I a

  let decode = function
    | B.I i -> i
    | _ -> raise (Invalid_argument "bad address")
end

type event =
  | Receive of Address.t * Bencode.t   (* received message *)
  | ConnectionUp (* connection is up again *)
  | ConnectionDown  (* connection was cut *)
  | Stop  (* stop the DHT *)

type t = {
  events : event Signal.t;
  address : int;
}

let __table = Hashtbl.create 15
let __count = ref 0

let create () =
  let t = {
    events = Signal.create ();
    address = !__count;
  } in
  incr __count;
  Hashtbl.add __table t.address t;  (* to access the node by its ID *)
  t

let get i =
  Hashtbl.find __table i

let send t addr msg =
  let t' = get addr in  (* target *)
  Signal.send t'.events (Receive (t.address, msg))

let events t = t.events

let sleep f = Lwt_unix.sleep f

let address_of t = t.address

let stop t =
  assert (Hashtbl.mem __table t.address);
  Signal.send t.events Stop;
  Hashtbl.remove __table t.address;
  () 
