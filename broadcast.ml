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

(** {1 Broadcasting} *)

module B = Bencode

module type S = sig
  module Dht : Chord.S

  val start_broadcast : ?log:bool -> Dht.t -> unit
    (** Enable broadcasting on this DHT instance *)

  val enable_log : on:out_channel -> Dht.t -> unit
    (** Enable log messages *)

  val broadcast : Dht.t -> Bencode.t -> unit
    (** Broadcast the value to every other node in the DHT *)

  val on_broadcast : Dht.t -> (Dht.ID.t * Bencode.t) Signal.t
    (** Received broadcasted values *)

  val sent : Dht.t -> Bencode.t Signal.t
    (** Messages broadcasted from this very node *)
end

module Make(Dht : Chord.S) = struct
  module Dht = Dht
  module Rpc = Dht.AsRPC

  type t = {
    on_broadcast : (Dht.ID.t * Bencode.t) Signal.t;
    sent : Bencode.t Signal.t;
  }

  (* broadcast the given message [msg], up to [limit_id] *)
  let _broadcast dht from_id limit_id msg =
    let local_id = Dht.id (Dht.local dht) in
    (* right child *)
    let right = ref None in
    for j = Dht.Config.dimension - 1 downto 0 do
      let finger = Dht.Topology.finger dht j in
      if !right = None
      && (not (Dht.ID.eq local_id (Dht.id finger)))
      && Dht.Topology.between_strict (Dht.id finger) local_id limit_id
        then right := Some finger
    done;
    match !right with
    | None -> ()   (* leaf of broadcast tree *)
    | Some right ->
      (* now select left child *)
      let left = ref None in
      for i = 0 to Dht.Config.dimension - 1 do
        let finger = Dht.Topology.finger dht i in
        if !left = None
        && (not (Dht.ID.eq local_id (Dht.id finger)))
        && Dht.Topology.between_strict (Dht.id finger) local_id (Dht.id right)
          then left := Some finger
      done;
      (* send broadcast message to right with the same limit *)
      let msg' = B.L
        [ B.S "broadcast";
          B.S (Dht.ID.to_string from_id);
          B.S (Dht.ID.to_string limit_id);
          msg ] in
      Rpc.notify dht (Dht.id right) msg';
      (* if left exists, also send it the broadcast message
        with [right] as the new limit *)
      match !left with
      | None -> ()
      | Some left ->
        let msg'' = B.L
          [ B.S "broadcast";
            B.S (Dht.ID.to_string from_id);
            B.S (Dht.ID.to_string (Dht.id right));
            msg ] in
        Rpc.notify dht (Dht.id left) msg''

  (* handle incoming messages, looking for incoming "broadcast" *)
  let _handle_received dht t (_, reply_tag, msg) =
    match msg with
    | B.L [ B.S "broadcast"; B.S from_id; B.S limit; msg' ] ->
      begin try
        let from_id = Dht.ID.of_string from_id in
        let limit_id = Dht.ID.of_string limit in
        (* deliver message if it was not sent be me *)
        (if not (Dht.ID.eq from_id (Dht.id (Dht.local dht)))
          then Signal.send t.on_broadcast (from_id, msg'));
        (* broadcast the message *)
        _broadcast dht from_id limit_id msg';
      with _ -> ()
      end;
      true
    | _ -> true

  let get_event =
    let inj = Dht.Mixtbl.access () in
    fun dht ->
      try Dht.Mixtbl.get ~inj dht "broadcast.event"
      with Not_found ->
        let t = {
          on_broadcast = Signal.create ();
          sent = Signal.create ();
        } in
        Signal.on (Rpc.received dht) (_handle_received dht t);
        Dht.Mixtbl.set ~inj dht "broadcast.event" t;
        t

  let enable_log ~on dht =
    let t = get_event dht in
    let local_id = Dht.id (Dht.local dht) in
    Signal.on t.on_broadcast
      (fun (from_addr, msg) ->
        Printf.fprintf on "[broadcast %s] received %s from %s\n"
          (Dht.ID.to_string local_id) (B.to_string msg) (Dht.ID.to_string from_addr);
        true);
    Signal.on t.sent
      (fun msg ->
        Printf.fprintf on "[broadcast %s] start broadcasting %s\n"
          (Dht.ID.to_string local_id) (B.to_string msg);
        true);
    ()

  (* setup broadcasting *)
  let start_broadcast ?(log=false) dht =
    ignore (get_event dht);
    (if log then enable_log ~on:stderr dht)
    
  (* broadcast this message to every node *)
  let broadcast dht msg =
    let t = get_event dht in
    let local_id = Dht.id (Dht.local dht) in
    Signal.send t.sent msg;
    _broadcast dht local_id local_id msg;
    ()

  let on_broadcast dht =
    (get_event dht).on_broadcast

  let sent dht = 
    (get_event dht).sent
end
