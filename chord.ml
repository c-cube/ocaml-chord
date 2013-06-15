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

(** {6 Chord DHT} *)

(** {2 Network} *)

module type NET = sig
  module Address : sig
    type t
      (** A network address (IP:Port, typically) *)

    val encode : t -> Bencode.t
      (** Serialize the address *)

    val decode : Bencode.t -> t
      (** May raise {! Invalid_argument} *)
  end

  val send : Address.t -> Bencode.t -> unit
    (** Send a string to an address *)
end

(** {2 Configuration} *)

module type CONFIG = sig
  val redundancy : int
    (** Number of nodes to return after a lookup (the number of successors
        of the query ID). Must be >= 1 *)

  val stabilize_frequency : int
    (** Frequency at whAich stabilization of immediate neighbors is performed,
        in seconds. This should be quite frequent, and more frequent
        than {! finger_frequency} *)

  val finger_frequency: int
    (** Frequency at which Chord fingers are refreshed *)

  val node_timeout : int
    (** After this amount of time (in seconds), a node that does not reply
        is considered dead *)

  val timeout : float
    (** Timeout for message replies *)
end

module ConfigDefault : CONFIG = struct
  let redundancy = 5

  let stabilize_frequency = 30

  let finger_frequency = 120

  let node_timeout = 300

  let timeout = 5.
end

(** {2 DHT state} *)

(** The DHT is a 120-bits addressing Chord network. This module provides a
    functor that is message-agnostic, and only works on events.

    @see {{: http://en.wikipedia.org/wiki/Chord_%28peer-to-peer%29 } wikipedia}.
    for a description of Chord. *)

(** Signature of the DHT *)
module type S = sig
  module Net : NET
  module Config : CONFIG

  type id = string
    (** A string that uniquely identifies a node on the DHT *)

  type address = Net.Address.t

  type t
    (** Instance of the DHT. This contains the state (routing table,
        fingers, etc.) of a node of the DHT. *)

  type node
   (** The representation of a node of the DHT. It relates an ID (a hash
        uniquely identifying the remote node in the DHT) to a network address
        and a node payload (private key, owner metadata, etc.) *)

  val random_id : unit -> id
    (** A fresh, unique ID usable on the network *)

  val create : ?id:id -> ?payload:string -> address -> t
    (** New DHT, using the given network node. If no ID is provided,
        a new random one is used. The address of the local node
        must be provided.
        [payload] is an optional string that is attached to the newly
        created node. *)

  val local : t -> node
    (** Node that represents this very DHT node *)

  val id : node -> id
    (** ID of the given DHT node *)

  val address : node -> address
    (** Address of the node *)

  val payload : node -> string
    (** Payload of a node *)

  val connect : t -> address -> id option Lwt.t
    (** Try to connect to the remote note, returns the ID of the
        node on success. *)

  val find_node : t -> id -> node option Lwt.t
    (** Returns the successor node of the given ID. It may fail, in
        which case [None] is returned. *)

  val send : t -> id -> Bencode.t -> unit
    (** Send the given message to the {! Config.redundancy} successors of
        the given ID *)

  val receive : t -> Net.Address.t -> Bencode.t -> unit
    (** Have the DHT process this incoming message. The address of the
        sender is also passed as a parameter. *)

  val tick : t -> unit
    (** Tick, should be called regularly (frequently enough, say,
        every second). It is used to check timeouts. *)

  (** {2 Register to events} *)

  val on_message : t -> (Bencode.t -> unit) -> unit
    (** Subscribe to generic messages sent to this node. It is the
        receiving-side counterpart to {!send}. *)

  type change_event =
    | Join of node
    | Part of node

  val on_change : t -> (change_event -> unit) -> unit
    (** Register to {!change_event}s. Not all join/parts are known to the
        local node, but it is still an interesting information (especially
        about immediate redundancy). *)
end

module Make(Net : NET)(Config : CONFIG) = struct
  module Net = Net
  module Config = Config

  module BI = Big_int

  let n = 160
    (** Number of bits in the ID space *)

  type id = string
    (** A string that uniquely identifies a node on the DHT *)

  type address = Net.Address.t

  type t = {
    local : node;
    mutable predecessor : node option;
    mutable successors : node list;  (* a list of the {! Config.redundancy} successors *)
    fingers : node option array; (* accelerate lookups *)
    replies : (int, float * (Bencode.t option -> unit)) Hashtbl.t; (* replies *)
    mutable reply_tag : int;  (* fresh tag for queries *)
    mutable listeners : (string -> unit) list;
    mutable change_listeners : (change_event -> unit) list;
    mutable next_stabilize : float;
    mutable next_fingers : float;
  } (** The local node *)

  and node = {
    n_id : BI.big_int;
    mutable n_address : address;
    mutable n_payload : string;
    mutable n_timeout : float;  (* time to leave *)
  } (** A node of the DHT *)

  and change_event =
    | Join of node
    | Part of node

  (** {2 Implementation} *)

  let _bound = BI.power_big_int_positive_int (BI.big_int_of_int 1) n

  let _modulo i = BI.mod_big_int i _bound

  (* [_plus_power_two i j] = [i + 2 ** j] *)
  let _plus_power_two i j =
    let two_power_j = BI.power_big_int_positive_int (BI.big_int_of_int 2) j in
    _modulo (BI.add_big_int i two_power_j)

  (* [between i j k] is true iff [i] is between [j] and [k] on the ring *)
  let _between i j k =
    if j <= k
      then (j <= i && i <= k)
      else (k <= i || i <= j)

  (* conversion string -> big int *)
  let id_of_string = BI.big_int_of_string

  (* conversion big int -> string *)
  let string_of_id = BI.string_of_big_int

  (* a unique tag to identify a request/reply message exchange *)
  let _get_reply_tag ~dht =
    let n = dht.reply_tag in
    dht.reply_tag <- n + 1;
    n

  (* send a message without reply to the given address *)
  let _send_no_reply ~dht addr msg =
    Net.send addr msg

  (* send a message, expecting a reply. The callback [k] will be
    called with the reply. *)
  let _send_wait_reply ~dht ~tag addr msg k =
    let module B = Bencode in
    (* prepare for answer (with a timeout) *)
    assert (not (Hashtbl.mem dht.replies tag));
    let ttl = Unix.gettimeofday () +. Config.timeout in
    Hashtbl.add dht.replies tag (ttl, k);
    (* send message *)
    Net.send addr msg;
    ()

  let node_to_bencode node =
    let module B = Bencode in
    B.L [B.S (string_of_id node.n_id);
         Net.Address.encode node.n_address;
         B.S node.n_payload]

  let _touch_node node =
    let now = Unix.gettimeofday () in
    node.n_timeout <- now +. float_of_int Config.node_timeout

  let node_of_bencode msg =
    let module B = Bencode in
    match msg with
    | B.L [B.S id; addr; B.S payload] ->
      let node = {
        n_id = id_of_string id;
        n_address = Net.Address.decode addr;
        n_payload = payload;
        n_timeout = 0.;
      } in
      _touch_node node;  (* expiration date *)
      node
    | _ -> invalid_arg "Chord.node_of_bencode: invalid B-encoded message"

  let _notify_join ~dht node =
    List.iter (fun f -> f (Join node)) dht.change_listeners

  let _notify_part ~dht node =
    List.iter (fun f -> f (Part node)) dht.change_listeners

  (* immediate successor *)
  let _get_successor ~dht = match dht.successors with
    | [] -> failwith "Chord._get_successor: no successor available"
    | s::_ -> s

  (* the known node that immediately precedes [id] *)
  let _closest_preceding_node ~dht id =
    let lower = BI.succ_big_int dht.local.n_id in
    let rec lookup i =
      if i = 0
        then dht.local  (* no other node than [dht.local] *)
      else match dht.fingers.(i) with
        | None -> lookup (i-1)
        | Some node ->
          if _between node.n_id lower id
            then node  (* node is the predecessor *)
            else lookup (i-1)
    in
    lookup (n-1)

  (** The wire format is B-encoded messages. At the top-level, the B-encode
      structures are composed of a constructor (indicating which method
      is called) or "reply", an optional "tag" for query/reply, and arguments.
      
      ---> find_node: ["find", tag, id]
      <---            ["reply", tag, bencoded node]
      
      ---> ping:  ["ping", tag]
      <---        ["reply", tag]

      ---> predecessor: ["predecessor", tag]
      <---              ["reply", tag, bencoded node]

      ---> notify:  ["notify"; bencoded node]

      ---> message: ["msg"; bencoded value]
      *)

  (* some (possibly remote) node wants to know the successor of [id], and
    calls the callback [k] with the answer. *)
  let _find_successor ~dht id k =
    let module B = Bencode in
    if id = dht.local.n_id
      then k (Some dht.local)  (* it's me! *)
    else if _between id dht.local.n_id (_get_successor dht).n_id
      then k (Some (_get_successor ~dht))  (* known successor *)
    else
      (* ask the highest preceding node *)
      let n' = _closest_preceding_node ~dht id in
      let tag = _get_reply_tag dht in
      let msg = B.L [ B.S "find";
                      B.I tag;
                      B.S (string_of_id id);
                    ] in
      (* send message and wait for reply *)
      _send_wait_reply ~dht ~tag n'.n_address msg
        (function
        | Some (B.L [B.S "reply"; _; node]) ->
          begin try (* decode node and give it to [k] *)
            let node = node_of_bencode node in
            k (Some node)
          with Invalid_argument _ -> k None
          end
        | _ -> k None)

  (* notify the given remote node that we think it's our successor *)
  let _notify ~dht addr =
    let module B = Bencode in
    let msg = B.L [B.S "notify"; node_to_bencode dht.local] in
    _send_no_reply ~dht addr msg;
    ()

  (* ping the given address to see if it answers. The answer is given to
      the callback [k] *)
  let _ping ~dht addr k =
    let module B = Bencode in
    let tag = _get_reply_tag ~dht in
    let msg = B.L [ B.S "ping"; B.I tag ] in
    _send_wait_reply ~dht ~tag addr msg k;
    ()

  (* reply to a "ping" message *)
  let _pong ~dht tag addr =
    let module B = Bencode in
    let msg = B.L [ B.S "reply"; B.I tag ] in
    _send_no_reply ~dht addr msg;
    ()

  (* what is youuuuur favorite colour^W predecessor? *)
  let _ask_predecessor ~dht addr k =
    let module B = Bencode in
    let tag = _get_reply_tag ~dht in
    let msg = B.L [ B.S "predecessor"; B.I tag ] in
    _send_wait_reply ~dht ~tag addr msg k

  (* send my predecessor to [addr] *)
  let _reply_predecessor ~dht tag addr =
    let module B = Bencode in
    match dht.predecessor with
    | None -> ()  (* do not reply :p *)
    | Some node ->
      let msg = B.L [B.S "reply"; B.I tag; node_to_bencode node] in
      _send_no_reply ~dht addr msg

  (* send a user-defined message to the given node *)
  let _send_user_message ~dht id msg =
    let module B = Bencode in
    (* first we need the address of the successor node *)
    _find_successor ~dht id
      (function
      | None -> ()  (* could not find ID *)
      | Some node ->
        (* found the recipient of the message, now send it the message *)
        let msg' = B.L [B.S "msg"; msg] in
        _send_no_reply ~dht node.n_address msg')

  (* we received a "notify" message from the given node *)
  let _handle_notify ~dht node =
    match dht.predecessor with
    | None ->
      dht.predecessor <- Some node  (* update predecessor *)
    | Some node'
      when _between node.n_id
        (BI.succ_big_int node'.n_id) (BI.pred_big_int dht.local.n_id) ->
      dht.predecessor <- Some node  (* update predecessor *)
    | _ -> ()

  (* handle incoming messages *)
  let _dispatch_msg ~dht sender msg =
    let module B = Bencode in
    match msg with
    | B.L [B.S "find"; B.I tag; B.S dest_id] ->
      (* ask for the node that is the immediate successor of [dest_id] *)
      let dest_id = id_of_string dest_id in
      _find_successor ~dht dest_id
        (function
        | Some node ->
          (* got a reply, forward it *)
          let msg' = B.L [B.S "found"; B.I tag; node_to_bencode node] in
          _send_no_reply ~dht sender msg'
        | None -> ()
        )
    | B.L [B.S "ping"; B.I tag ] ->
      (* must reply to the "ping" *)
      _pong ~dht tag sender
    | B.L [B.S "notify"; node] ->
      let node = node_of_bencode node in
      _handle_notify ~dht node
    | B.L [B.S "predecessor"; B.I tag] ->
      _reply_predecessor ~dht tag sender
    | _ -> ()

  (* check whether new successors have joined *)
  let _stabilize ~dht =
    () (* TODO *)

  (* check whether the predecessor is alive *)
  let _check_predecessor ~dht =
    match dht.predecessor with
    | None -> ()
    | Some node ->
      let k = function
        | None ->
          dht.predecessor <- None;  (* predecessor died *)
          _notify_part ~dht node
        | Some _ -> ()
      in
      (* ping the predecessor *)
      _ping ~dht node.n_address k

  (* update the fingers table *)
  let _fix_fingers ~dht =
    () (* TODO *)

  (* timeouts for request/reply messages *)
  let _check_timeouts ~dht =
    () (* TODO *)

  (* check timeouts *)
  let _tick ~dht =
    let now = Unix.gettimeofday () in
    (if dht.next_stabilize > now
      then begin
        dht.next_stabilize <- now +. (float_of_int Config.stabilize_frequency);
        _stabilize ~dht;
        _check_predecessor ~dht;
      end);
    (if dht.next_fingers > now
      then begin
        dht.next_fingers <- now +. (float_of_int Config.finger_frequency);
        _fix_fingers ~dht;
      end);
    (* simple message timeouts *)
    _check_timeouts ~dht;
    ()

  (** {2 Public interface} *)

  (* random int between 0 and 2^160-1 *)
  let _random_id () =
    let i = ref BI.zero_big_int in
    let sixteen = BI.big_int_of_int 16 in
    for j = 0 to (n/16) - 1 do
      let r = Random.int (1 lsl 16) in
      i := BI.add_big_int (BI.mult_big_int sixteen !i) (BI.big_int_of_int r)
    done;
    !i

  (* create a new DHT node *)
  let create ?id ?(payload="") address =
    let id = match id with
      | Some i -> id_of_string i
      | None -> _random_id ()
    in
    (* local node *)
    let local = {
      n_id = id;
      n_address = address;
      n_payload = payload;
      n_timeout = infinity;
    } in
    let now = Unix.gettimeofday () in
    let dht = {
      local;
      predecessor=None;
      successors=[local];
      reply_tag = 0;
      replies = Hashtbl.create 128;
      fingers = Array.create n None;
      listeners = [];
      change_listeners = [];
      next_stabilize = now;
      next_fingers = now +. 10.;
    } in
    dht

  (* Get the ID of the DHT. Returns a copy, to be sure that
    it's not modified *)
  let local dht =
    dht.local

  let random_id () =
    string_of_id (_random_id ())

  let id node = string_of_id node.n_id

  let address node = node.n_address

  let payload node = node.n_payload

  let connect dht addr =
    (* TODO: fix successor, fix fingers soon *)
    failwith "connect: not implemented"

  let find_node dht id =
    let id = id_of_string id in
    let future, promise = Lwt.wait () in
    _find_successor ~dht id (Lwt.wakeup promise);
    future

  let send dht id msg =
    failwith "send: not implemented"

  let receive dht sender msg =
    try (* handle the message *)
      _dispatch_msg ~dht sender msg
    with Invalid_argument e ->
      Printf.eprintf "Chord: invalid argument: %s" e;
      ()

  let tick dht = _tick ~dht

  let on_message id k =
    failwith "on_message: not implemented"

  let on_change dht k =
    failwith "on_change: not implemented"
end
