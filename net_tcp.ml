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
    B.L [ B.S (Unix.string_of_inet_addr addr); B.I port ]

  let decode = function
    | B.L [ B.S addr; B.I port ] ->
      begin try
        let addr = Unix.inet_addr_of_string addr in
        addr, port
      with _ -> raise (Invalid_argument "bad address")
      end
    | _ -> raise (Invalid_argument "bad address")

  let eq a b = B.eq (encode a) (encode b)

  let hash a = B.hash (encode a)

  (* change port *)
  let with_port (addr,_) port = (addr, port)

  let to_string (addr, port) =
    Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

  let to_sockaddr (addr, port) =
    Unix.ADDR_INET (addr, port)

  let of_sockaddr = function
    | Unix.ADDR_INET (a,i) -> a, i
    | _ -> failwith "unexpected Unix address"
end

(** Hashtables on addresses *)
module AddrHashtbl = Hashtbl.Make(struct
  type t = Address.t
  let equal = Address.eq
  let hash = Address.hash
end)

let mk_addr addr port =
  Unix.inet_addr_of_string addr, port

let mk_local_addr port =
  Unix.inet_addr_any, port

let mk_by_name host port =
  Lwt_unix.gethostbyname host >>= fun host ->
  if Array.length host.Unix.h_addr_list = 0
    then Lwt.return_none
    else
      let addr = host.Unix.h_addr_list.(0), port in
      Lwt.return (Some addr)

type event =
  | Receive of Address.t * Bencode.t   (* received message *)
  | Stop  (* stop the DHT *)

type conn_cache = {
  cc_table : (float * string option Lwt_queue.t) AddrHashtbl.t;
  cc_size : int;
} (** Cache of outgoing connections *)

let mk_conn_cache size =
  {
    cc_table = AddrHashtbl.create size;
    cc_size = size;
  }

type t = {
  events : event Signal.t;
  sent : (Address.t * Bencode.t) Signal.t;
  port : int;
  mutable listen_thread : unit Lwt.t option;
  client_threads : unit Lwt.t AddrHashtbl.t;
  conn_cache : conn_cache;  (* cache of connections *)
  mutable stop : bool;
  on_stop : unit Lwt_condition.t;
} (** Network layer that uses sockets *)

let _log format =
  let b = Buffer.create 15 in
  Printf.kbprintf
    (fun b ->
      Lwt.ignore_result (Lwt_io.printl (Buffer.contents b)))
    b format

let enable_log ?(on=stderr) t =
  Signal.on t.sent
    (fun (to_, msg) ->
      Printf.fprintf on "[net]: send %s to %s\n"
        (B.pretty_to_str msg) (Address.to_string to_);
      flush on;
      true);
  Signal.on t.events
    (function
      | Receive (from_, msg) ->
        Printf.fprintf on "[net]: receive %s from %s\n"
          (B.pretty_to_str msg) (Address.to_string from_);
        flush on;
        true
      | Stop ->
        Printf.fprintf on "[net]: stop\n";
        flush on;
        false);
  ()

(* create a socket with the given address
  TODO: option for ipv6 *)
let mk_socket () =
  Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0

(* remove entry of client *)
let _remove_client t addr =
  AddrHashtbl.remove t.client_threads addr

let rec _read_bencode ~socket ~decoder ~buf = 
  Lwt_unix.read socket buf 0 (String.length buf) >>= fun n ->
  if n = 0
    then
      Lwt_unix.close socket >>= fun () ->
      Lwt.return_none
    else match B.parse decoder buf 0 n with
      | B.ParseError msg ->
        _log "parse error";
        Lwt_unix.close socket >>= fun () ->
        Lwt.return_none
      | B.ParsePartial ->
        _read_bencode ~socket ~decoder ~buf (* read more *)
      | B.ParseOk msg ->
        _log "read incoming msg %s" (B.pretty_to_str msg);
        Lwt.return (Some msg)
  

(* listen for messages from client, reading messages from the socket *)
let rec _listen_client t ~socket ~decoder ~buf addr =
  _read_bencode ~socket ~decoder ~buf >>= fun b ->
  match b with
  | None ->
    _remove_client t addr;
    Lwt.return_unit
  | Some msg ->
    _log "received %s from client" (B.pretty_to_str msg);
    Signal.send t.events (Receive (addr, msg));
    _listen_client t ~socket ~decoder ~buf addr

(* handle given new client *)
let _handle_client t socket addr =
  let addr = Address.of_sockaddr addr in
  let decoder = B.mk_decoder () in
  let buf = String.make 256 ' ' in
  _read_bencode ~socket ~decoder ~buf >>= fun b ->
  match b with
  | Some (B.I port) ->
    let addr = Address.with_port addr port in
    Lwt_io.printl ("connection from " ^ Address.to_string addr) >>= fun () ->
    (* handle incoming connection *)
    let fut = _listen_client t ~socket ~decoder ~buf addr in
    AddrHashtbl.add t.client_threads addr fut;
    Lwt.return_unit
  | _ -> Lwt.return_unit

(* main thread that listens to incoming connections *)
let _listen t port =
  let addr = mk_local_addr port in
  let s = mk_socket () in
  Lwt_unix.bind s (Address.to_sockaddr addr);
  Lwt_unix.listen s 1024;
  let rec accept () =
    Lwt.pick
      [ (Lwt_unix.accept s >>= fun (s', addr') ->
        _handle_client t s' addr')
      ; Lwt_condition.wait t.on_stop  (* stop! *)
      ]
    >>= fun () ->
    if not t.stop
      then accept ()  (* accept next connection *)
      else Lwt.return_unit
  in
  accept ()

(* remove least recently used connection from cache *)
let _make_room_cache cache =
  if AddrHashtbl.length cache.cc_table = cache.cc_size then
    let _, oldest = AddrHashtbl.fold
      (fun addr (last, q) (old_last, oldest) ->
        match oldest with
        | None -> last, Some (addr, q)
        | _ when last > old_last -> old_last, oldest
        | _ -> last, Some (addr, q))
      cache.cc_table (~-. infinity, None)
    in
    match oldest with
    | None -> assert false
    | Some (addr, q) ->
      (* close this connection by asking its thread to stop *)
      _log "make room cache: remove conn to %s" (Address.to_string addr);
      AddrHashtbl.remove cache.cc_table addr;
      Lwt_queue.push q None

(* writer thread *)
let _write_thread sock queue =
  (* get next item to write *)
  let rec get_item () =
    _log "_write_thread: wait for message to send";
    Lwt_queue.pop queue >>=
    function
    | None ->
      Lwt_unix.close sock
    | Some str ->
      _log "_write_thread: send %s" str;
      write_item str 0 (String.length str)
  (* write the given buffer (n bytes remaining) *)
  and write_item buf i n =
    if n = 0
      then get_item ()
      else
        Lwt_unix.write sock buf i n >>= fun j ->
        write_item buf (i+j) (n-j)
  in
  get_item ()

(* find a connection to address, or create one *)
let _get_conn t addr =
  try
    let _, q = AddrHashtbl.find t.conn_cache.cc_table addr in
    Lwt.return q
  with Not_found ->
    _make_room_cache t.conn_cache;
    (* connect to the remote address *)
    let s = mk_socket () in
    Lwt_unix.connect s (Address.to_sockaddr addr) >>= fun () ->
    let q = Lwt_queue.create () in
    (* start a thread to write from queue to socket *)
    let worker = _write_thread s q in
    Lwt_queue.push q (Some (B.to_string (B.I t.port)));
    (* save the connection *)
    let now = Unix.gettimeofday () in
    AddrHashtbl.add t.conn_cache.cc_table addr (now, q);
    Gc.finalise (fun _ -> Lwt.cancel worker) q;
    Lwt.return q

(* send message *)
let send t addr msg =
  Lwt.ignore_result
    (Lwt.catch
      (fun () ->
        (_get_conn t addr >>= fun q ->
        let str = B.to_string msg in
        Lwt_queue.push q (Some str);
        Lwt.return_unit))
      (function
        | Unix.Unix_error _ as e ->
          _log "error: %s" (Printexc.to_string e);
          Lwt.return_unit
        | e -> raise e))

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
    client_threads = AddrHashtbl.create 5;
    conn_cache = mk_conn_cache 16;  (* TODO option for size *)
    stop = false;
    on_stop = Lwt_condition.create ();
  } in
  try
    t.listen_thread <- Some (_listen t port);
    (if log then enable_log t);
    Some t
  with Unix.Unix_error _ ->
    None  (* could not listen *)

let stop t =
  assert (not t.stop);
  t.stop <- true;
  Lwt_condition.broadcast t.on_stop ();
  ()

let events t = t.events

let sent t = t.sent

let port t = t.port

let call_in time f =
  let fut = Lwt_unix.sleep time in
  Lwt.on_success fut f

let fmt fmt addr =
  Format.pp_print_string fmt (Address.to_string addr)

let to_string = Address.to_string
