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

  val store : t -> key -> value -> unit
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

  (* handle a "get" message *)
  let _handle_get ~store tag key =
    try
      let cell = IHashtbl.find store.table key in
      if is_valid cell
        then
          let msg = B.S cell.pc_value in
          Rpc.reply (DHT.rpc store.dht) tag msg
    with Not_found ->
      ()

  (* handle a "store" message *)
  let _handle_store ~store tag key value =
    if IHashtbl.mem store.table key
      then  (* collision; signal failure *)
        let msg = B.I 0 in
        Rpc.reply (DHT.rpc store.dht) tag msg
      else begin
        (* store key/value *)
        let cell = mk_cell ~store key value in
        IHashtbl.add store.table key cell;
        (* signal success *)
        let msg = B.I 1 in
        Rpc.reply (DHT.rpc store.dht) tag msg;
        (* TODO: notify a few successors to replicate the key/value *)
        Signal.send store.on_store (key,value);
      end

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
    true

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
    (* TODO: check for garbage collected bindings *)
    Signal.on (Rpc.received (DHT.rpc dht)) (_handle_message ~store);
    store

  let get store key =
    
    failwith "Chord.Store.get: not implemented"

  let store store key value =
    failwith "Chord.Store.set: not implemented"

  (* iterate on pairs that are still alive *)
  let iter store k =
    IHashtbl.iter
      (fun _ pair -> if is_valid pair then k pair.pc_key pair.pc_value)
      store.table

  let on_timeout store = store.on_timeout

  let on_store store = store.on_store
end

