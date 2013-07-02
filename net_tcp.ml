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

module B = Bencode

let (>>=) = Lwt.(>>=)

module Address = struct
  type t = Unix.inet_addr * int
    (** Adresses are internet addresses, + port *)

  let encode (addr, port) =
    B.L [ B.S (Unix.string_of_inet_addr); B.I port ]

  let decode = function
    | B.L [ B.S addr; B.I port ] ->
      begin try
        let addr = Unix.inet_addr_of_string addr in
        addr, port
      with _ -> raise (Invalid_argument "bad address")
      end
    | _ -> raise (Invalid_argument "bad address")

  let eq a b = encode a = encode b

  let to_string (addr, port) =
    Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

  let to_sockaddr (addr, port) =
    Unix.ADDR_INET (addr, port)
end

let mk_addr addr port =
  Unix.inet_addr_of_string addr, port

let mk_local_addr port =
  Unix.inet_addr_any, port

type event =
  | Receive of Address.t * Bencode.t   (* received message *)
  | Stop  (* stop the DHT *)

(* TODO: a cache of connections to other nodes *)

type t = {
  events : event Signal.t;
  sent : (Address.t * Bencode.t) Signal.t;
  port : int;
  mutable listen_thread : unit Lwt.t option;
  client_threads : (Address.t, unit Lwt.t) Hashtbl.t;
  mutable stop : bool;
  on_stop : unit Lwt_condition.t;
} (** Network layer that uses sockets *)

let enable_log ?(on=stderr) t =
  Signal.on t.sent
    (fun (to_, msg) ->
      Printf.fprintf on "[net %s]: send %s to %s\n"
        (Address.to_string t.address) (B.pretty_to_str msg) (Address.to_string to_);
      true);
  Signal.on t.events
    (function
      | Receive (from_, msg) ->
        Printf.fprintf on "[net %s]: receive %s from %s\n"
          (Address.to_string t.address) (B.pretty_to_str msg) (Address.to_string from_);
        true
      | Stop ->
        Printf.fprintf on "[net %s]: stop\n" (Address.to_string t.address);
        false);
  ()

(* create a socket with the given address
  TODO: option for ipv6 *)
let mk_socket () =
  Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0

(* handle given client *)
let _handle_client t s addr = (* TODO *)
  failwith "Net_tcp._handle client: not implemented"

(* main thread that listens to incoming connections *)
let _listen t port =
  let addr = mk_local_addr port in
  let s = mk_socket () in
  Lwt_unix.bind s addr;
  let rec accept () =
    Lwt.pick
      [ Lwt_unix.accept s >>= fun (s', addr') ->
        (* handle incoming connection *)
        let fut = _handle_client t s' addr' in
        Hashtbl.add t.client_threads addr' fut;
      ; Lwt_condition.wait t.on_stop  (* stop! *)
      ]
    >>= fun () ->
    if not t.stop
      then accept ()  (* accept next connection *)
  in
  accept ()

let create ?(log=false) ?port () =
  (* random port if none is specified *)
  let port = match port with
    | Some p -> p
    | None -> (Random.int 5000) + 1024
  in
  let rec t = {
    events = Signal.create ();
    sent = Signal.create ();
    port;
    listen_thread = None;
    client_threads = Hashtbl.create 5;
    stop = false;
    on_stop = Lwt_condition.create ();
  } in
  t.listen_thread <- Some (_listen t port);
  (if log then enable_log t);
  t

let stop t =
  assert (not t.stop);
  t.stop <- true;
  Lwt_condition.broadcast t.on_stop ();
  ()

let events t = t.events

let sent t = t.sent
