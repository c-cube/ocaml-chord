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

(** {6 Simple messaging program} *)

module N = Net_tcp
module Dht = Chord.Make(N)(Chord.ConfigDefault)
module B = DhtBroadcast.Make(Dht)

let (>>=) = Lwt.(>>=)

let port = ref None
let enable_debug = ref false

let addresses = ref []

let options =
  [ "-p", Arg.Int (fun i -> port := Some i), "port to listen on"
  ; "-debug", Arg.Set enable_debug, "enable debugging messages"
  ]

(* read input from the user *)
let rec read_input dht =
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | None
  | Some ("\r\n" | "" | "\n") -> Lwt.return_unit
  | Some line ->
    Lwt_io.printl ("send: \'" ^ line ^ "\'") >>= fun () ->
    let msg = Bencode.S line in
    B.broadcast dht msg;
    read_input dht

(* print broadcasted messages to stdout *)
let rec print_messages dht =
  B.recv dht >>= fun (id, msg) ->
  begin match msg with
  | Bencode.S line ->
    let output = Printf.sprintf "<%s> %s" (Dht.ID.to_string id) line in
    Lwt_io.printl output
  | _ -> Lwt.return_unit
  end
  >>= fun () ->
  print_messages dht

(* connect to the given address *)
let connect_to dht (host,port) =
  Lwt_io.printl ("try to resolve " ^ host ^ " ... ") >>= fun () ->
  (* connect to the given address(es) *)
  N.mk_by_name host port >>= fun addr ->
  match addr with
  | None -> Lwt_io.printl ("could not resolve host " ^ host)
  | Some addr ->
    let output = Format.sprintf "connect to addr %s" (N.to_string addr) in
    Lwt_io.printl output >>= fun () ->
    Dht.connect dht addr >>= fun ret -> match ret with
    | None -> Lwt_io.printl "unable to contact node"
    | Some n ->
      Lwt_io.printl ("connected to node " ^ Dht.ID.to_string (Dht.id n))

(* start everything *)
let start_client ?port addresses =
  let log = !enable_debug in
  let net = N.create ?port ~log () in
  match net with
  | None -> Lwt_io.printl "could not create network node"
  | Some net ->
    let output = Format.sprintf "listen on port %d" (N.port net) in
    Lwt_io.printl output >>= fun () ->
    let dht = Dht.create ~log net in
    B.enable_log dht;
    List.iter (fun a -> Lwt.ignore_result (connect_to dht a)) addresses;
    let tasks = [read_input dht; print_messages dht] in
    Lwt.pick tasks

let parse_addr addr =
  Scanf.sscanf addr "%s@: %d" (fun host port -> host, port)

let parse_opts () =
  Arg.parse options
    (fun s -> addresses := parse_addr s :: !addresses)
    "client [-h host] [-p port]";
  ()

let _ =
  parse_opts ();
  Random.self_init ();
  Lwt_main.run (start_client ?port:!port !addresses);
  ()
