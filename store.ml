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

module B = Bencode

module type S = sig
  module DHT : Chord.S

  type id = DHT.ID.t
  type key = id
  type value = string

  type t
    (** A key/value distributed store, on top of the given {! DHT} module *)

  val create : ?gc:int -> DHT.t -> t
    (** Create a key/value store that uses this DHT. The optional [gc]
        parameter is used to remove key/value pairs that have not been
        accessed for [gc] seconds. By default, key/value pairs are kept
        forever. *)

  val get : t -> key -> value option Lwt.t
    (** Get the value associated to this key, if it can be found *)

  val store : t -> key -> value -> bool Lwt.t
    (** Store the given [key -> value] in the DHT *)

  val iter : t -> (key -> value -> unit) -> unit
    (** Key/value pairs stored in the given store *)

  val on_timeout : t -> (key * value) Signal.t

  val on_store : t -> (key * value) Signal.t
end

module Make(DHT : Chord.S) = struct
  module DHT = DHT
  module Rpc = DHT.Rpc

  type id = DHT.ID.t
  type key = id
  type value = string

  module IHashtbl = Hashtbl.Make(struct
    type t = id
    let equal = DHT.ID.eq
    let hash = DHT.ID.hash
  end)

  type t = {
    dht : DHT.t;
    table : pair_cell IHashtbl.t;
    gc : float;  (* GC timeout *)
    on_timeout : (key * value) Signal.t;
    on_store : (key * value) Signal.t;
  }

  and pair_cell = {
    pc_key : key;
    pc_value : string;
    mutable pc_ttl : float;  (* time to live *)
  } (** A key/value pair *)

  let mk_cell ~store key value =
    let cell = {
      pc_key = key;
      pc_value = value;
      pc_ttl = Unix.gettimeofday () +. store.gc;
    } in
    cell

  (* is this key/value pair still valid? *)
  let is_valid pair_cell =
    let now = Unix.gettimeofday () in
    pair_cell.pc_ttl > now

  let _touch_cell ~store cell =
    cell.pc_ttl <- Unix.gettimeofday () +. store.gc

  (* handle a "get" message *)
  let _handle_get ~store tag key =
    try
      let cell = IHashtbl.find store.table key in
      if is_valid cell
        then begin
          let msg = B.S cell.pc_value in
          (* value still used *)
          _touch_cell ~store cell;
          Rpc.reply (DHT.rpc store.dht) tag msg
        end
    with Not_found ->
      ()

  (* handle a "store" message *)
  let _handle_store ~store tag key value =
    if IHashtbl.mem store.table key
      then  (* collision; signal failure *)
        let msg = B.I 0 in
        Rpc.reply (DHT.rpc store.dht) tag msg
      else begin
        DHT.log store.dht "store: put %s -> %s" (DHT.ID.to_string key) value;
        (* store key/value *)
        let cell = mk_cell ~store key value in
        IHashtbl.add store.table key cell;
        (* signal success *)
        let msg = B.I 1 in
        Rpc.reply (DHT.rpc store.dht) tag msg;
        (* TODO: notify a few successors to replicate the key/value *)
        Signal.send store.on_store (key,value);
      end

  (* protocol:
  
     get the value for this key
     -->  [ "store.get"; key ]
     <--  [ value ]

     returns 0 on failure, 1 on success
     -->  [ "store.store"; key; value]
     <--  [ 0 | 1 ]
  *)

  let _handle_message ~store (sender, tag, msg) =
    begin match msg, tag with
    | B.L [ B.S "store.get"; B.S key ], Some tag ->
      let key = DHT.ID.of_string key in
      _handle_get ~store tag key
    | B.L [ B.S "store.store"; B.S key; B.S value ], Some tag ->
      let key = DHT.ID.of_string key in
      _handle_store ~store tag key value
    | _ -> ()
    end;
    not (DHT.stopped store.dht)

  let rec _check_gc ~store =
    let now = Unix.gettimeofday () in
    (* find bindings that have expired *)
    let l = ref [] in
    IHashtbl.iter
      (fun key cell ->
        if cell.pc_ttl < now
          then l := cell :: !l)
      store.table;
    List.iter
      (fun cell ->
        IHashtbl.remove store.table cell.pc_key;
        Signal.send store.on_timeout (cell.pc_key, cell.pc_value))
      !l;
    DHT.log store.dht "store: GC done (%d collected)" (List.length !l);
    (* schedule next GC pass *)
    DHT.Net.call_in
      (store.gc /. 2.)
      (fun () -> _check_gc ~store)

  let create ?gc dht =
    let gc = match gc with
      | None -> infinity
      | Some t -> float_of_int t
    in
    let store = {
      dht;
      table = IHashtbl.create 128;
      gc;
      on_timeout = Signal.create ();
      on_store = Signal.create ();
    } in
    (* listen for incoming RPC messages *)
    Signal.on (Rpc.received (DHT.rpc dht)) (_handle_message ~store);
    (* check GC *)
    _check_gc ~store;
    store

  let get store key =
    let fut = DHT.find_node store.dht key in
    Lwt.bind fut
      (function
        | None -> Lwt.return_none
        | Some node ->
          let addr = List.hd (DHT.addresses node) in
          let msg = B.L [ B.S "store.get"; B.S (DHT.ID.to_string key)] in
          let fut' = Rpc.send ~timeout:5. (DHT.rpc store.dht) addr msg in
          Lwt.bind fut'
            (function
              | Some (B.S value) -> Lwt.return (Some value)
              | _ -> Lwt.return_none))

  let store store key value =
    let fut = DHT.find_node store.dht key in
    Lwt.bind fut
      (function
        | None -> Lwt.return_false
        | Some node ->
          let addr = List.hd (DHT.addresses node) in
          let msg = B.L [ B.S "store.set"; B.S (DHT.ID.to_string key); B.S value] in
          let fut' = Rpc.send ~timeout:5. (DHT.rpc store.dht) addr msg in
          Lwt.bind fut'
            (function
              | Some (B.I 0) -> Lwt.return_false
              | Some (B.I 1) -> Lwt.return_true
              | _ -> Lwt.return_false))

  (* iterate on pairs that are still alive *)
  let iter store k =
    IHashtbl.iter
      (fun _ pair -> if is_valid pair then k pair.pc_key pair.pc_value)
      store.table

  let on_timeout store = store.on_timeout

  let on_store store = store.on_store
end

