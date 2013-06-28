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
  type t = string
    (** Adresses are just a handle to some thread *)

  let encode a = B.S a

  let decode = function
    | B.S a -> a
    | _ -> raise (Invalid_argument "bad address")

  let eq a b = a = b
end

type event =
  | Receive of Address.t * Bencode.t   (* received message *)
  | ConnectionUp (* connection is up again *)
  | ConnectionDown  (* connection was cut *)
  | Stop  (* stop the DHT *)

type t = {
  events : event Signal.t;
  sent : (Address.t * Bencode.t) Signal.t;
  address : string;
}

let __table = Hashtbl.create 15
let __count = ref 0

let enable_log ?(on=stderr) t =
  Signal.on t.sent
    (fun (to_, msg) ->
      Printf.fprintf on "[net %s]: send %s to %s\n"
        t.address (B.pretty_to_str msg) to_;
      true);
  Signal.on t.events
    (function
      | Receive (from_, msg) ->
        Printf.fprintf on "[net %s]: receive %s from %s\n"
          t.address (B.pretty_to_str msg) from_;
        true
      | Stop ->
        Printf.fprintf on "[net %s]: stop\n" t.address;
        false
      | _ -> true);

  ()

let create ?(log=false) () =
  let t = {
    events = Signal.create ();
    sent = Signal.create ();
    address = Printf.sprintf "addr_%d" !__count;
  } in
  incr __count;
  Hashtbl.add __table t.address t;  (* to access the node by its ID *)
  (if log then enable_log t);
  t

let get i =
  Hashtbl.find __table i

let send t addr msg =
  let t' = get addr in  (* target *)
  Signal.send t.sent (addr,msg);
  Signal.send t'.events (Receive (t.address, msg));
  ()

let events t = t.events

let call_in time f =
  let fut = Lwt_unix.sleep time in
  Lwt.on_success fut f

let address_of t = t.address

let stop t =
  assert (Hashtbl.mem __table t.address);
  Signal.send t.events Stop;
  Hashtbl.remove __table t.address;
  () 

let send_node n1 n2 msg =
  send n1 (address_of n2) msg

let sent t = t.sent
