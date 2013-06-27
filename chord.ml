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

(** The network module is an abstraction over communication with other nodes.
    It allows to designates other entities via {b addresses} (for instance,
    process IDs, or IP+port addresses), and to send and receive messages
    as B-encoded data. A primitive to wait is also provided.

    A typical implementation may use TCP connections to send and receive
    messages. *)

module type NET = sig
  module Address : sig
    type t
      (** A network address (IP:Port, typically) *)

    val encode : t -> Bencode.t
      (** Serialize the address *)

    val decode : Bencode.t -> t
      (** May raise {! Invalid_argument} *)

    val eq : t -> t -> bool
  end

  type t
    (** A node on the network *)

  val send : t -> Address.t -> Bencode.t -> unit
    (** Send a string to an address *)

  type event =
    | Receive of Address.t * Bencode.t   (* received message *)
    | ConnectionUp (* connection is up again *)
    | ConnectionDown  (* connection was cut *)
    | Stop  (* stop the DHT *)

  val events : t -> event Signal.t
    (** Signal transmitting events that occur on the network *)

  val sleep : float -> unit Lwt.t
    (** Sleep for the given amount of seconds *)
end

(** {2 RPC} *)

(** This provides a lightweight RPC mechanism on top of a {!NET}
    implementation and B-encoded messages. *)

module type RPC = sig
  module Net : NET

  type address = Net.Address.t

  type t
    (** A RPC system *)

  type reply_tag
    (** A tag used to reply to messages *)

  val create : ?frequency:float -> Net.t -> t
    (** Create an instance of the RPC system, which can send and receive
        remote function calls using the [Net.t] instance.
        [frequency] is the frequency, in seconds, at which the
        RPC system checks whether some replies timed out. *)

  val notify : t -> address -> Bencode.t -> unit
    (** Send a message without expecting a reply *)

  val send : t -> ?timeout:float -> address -> Bencode.t -> Bencode.t option Lwt.t
    (** Send a message, expecting a reply *)

  val reply : t -> reply_tag -> Bencode.t -> unit
    (** Reply to the message whose tag is given *)

  val received : t -> (address * reply_tag option * Bencode.t) Signal.t
    (** Signal incoming messages. The signal transmits the sender's
        address, a reply tag (in case the sender expected a reply)
        and the message itself *)

  val stop : t -> unit
    (** Disable all threads and active processes *)
end

module MakeRPC(Net : NET) : RPC with module Net = Net = struct
  module Net = Net

  type address = Net.Address.t 

  type reply_tag = {
    rt_count : int;
    rt_address : address;
  } (* stores information necessary to reply to a message *)

  type t = {
    net : Net.t;
    frequency : float;
    mutable count : int;
    mutable stop : bool;
    received : (address * reply_tag option * Bencode.t) Signal.t;
    callbacks : (int, (float * Bencode.t option Lwt.u)) Hashtbl.t;
  } (** The RPC system *)

  module B = Bencode

  (* check whether some callbacks timed out *)
  let check_timeouts rpc =
    let to_remove = ref [] in
    let now = Unix.gettimeofday () in
    (* find callbacks that have expired *)
    Hashtbl.iter
      (fun i (ttl, promise) ->
        if ttl < now
          then to_remove := (i, promise) :: !to_remove)
      rpc.callbacks;
    (* remove all such callbacks *)
    List.iter
      (fun (i, promise) ->
        Hashtbl.remove rpc.callbacks i;
        Lwt.wakeup promise None)
      !to_remove;
    ()

  (* wait some time, then check timeouts and loop *)
  let rec poll rpc =
    let (>>=) = Lwt.(>>=) in
    if rpc.stop
      then Lwt.return_none
      else begin
        Net.sleep rpc.frequency
        >>= fun () ->
        check_timeouts rpc;
        poll rpc
      end

  let stop rpc =
    rpc.stop <- true

  (* handle network event *)
  let handle_event rpc ev = match ev with
    | Net.Stop -> stop rpc; false
    | Net.ConnectionUp
    | Net.ConnectionDown -> true
    | Net.Receive (addr, msg) ->
      begin match msg with
        | B.L [ B.S "ntfy"; msg' ] ->
          Signal.send rpc.received (addr, None, msg')
        | B.L [ B.S "msg"; B.I i; msg' ] ->
          let reply_tag = { rt_count = i; rt_address = addr; } in
          Signal.send rpc.received (addr, Some reply_tag, msg')
        | B.L [ B.S "reply"; B.I i; msg' ] ->
          begin try
            (* find which promise corresponds to this reply *)
            let _, promise = Hashtbl.find rpc.callbacks i in
            Hashtbl.remove rpc.callbacks i;
            Lwt.wakeup promise (Some msg');
          with Not_found -> ()
          end
        | _ ->
          Printf.eprintf "ill-formed RPC message: %s\n" (B.pretty_to_str msg)
      end;
      true

  (* create a new RPC system *)
  let create ?(frequency=2.0) net =
    let rpc = {
      net;
      frequency;
      count = 1;
      stop = false;
      received = Signal.create ();
      callbacks = Hashtbl.create 15;
    } in
    Lwt.ignore_result (poll rpc);
    Signal.on
      (Net.events rpc.net)
      (fun e -> handle_event rpc e);
    rpc

  let notify rpc addr msg =
    (if rpc.stop then failwith "RPC system stopped");
    let msg = B.L [ B.S "ntfy"; msg ] in
    Net.send rpc.net addr msg

  let send rpc ?timeout addr msg =
    (if rpc.stop then failwith "RPC system stopped");
    (* future for the answer, put it in hashtable *)
    let future, promise = Lwt.wait () in
    let n = rpc.count in
    rpc.count <- n + 1;
    let ttl = match timeout with
      | None -> infinity
      | Some t -> (assert (t> 0.); Unix.gettimeofday () +. t)
    in
    Hashtbl.add rpc.callbacks n (ttl, promise);
    (* send message wrapped in metadata *)
    let msg' = B.L [ B.S "msg"; B.I n; msg ] in
    Net.send rpc.net addr msg';
    future

  let reply rpc tag msg =
    let msg' = B.L [ B.S "reply"; B.I tag.rt_count; msg ] in
    Net.send rpc.net tag.rt_address msg'

  let received rpc =
    rpc.received
end

(** {2 Configuration} *)

(** This module contains values that parametrize the DHT's behavior. *)

module type CONFIG = sig
  val redundancy : int
    (** Number of nodes to return after a lookup (the number of successors
        of the query ID). Must be >= 1 *)

  val stabilize_frequency : float
    (** Frequency at whAich stabilization of immediate neighbors is performed,
        in seconds. This should be quite frequent, and more frequent
        than {! finger_frequency} *)

  val finger_frequency: float
    (** Frequency at which Chord fingers are refreshed *)

  val node_timeout : float
    (** After this amount of time (in seconds), a node that does not reply
        is considered dead *)

  val timeout : float
    (** Timeout for message replies (in seconds) *)
end

module ConfigDefault : CONFIG = struct
  let redundancy = 5

  let stabilize_frequency = 15.

  let finger_frequency = 10.

  let node_timeout = 300.

  let timeout = 3.
end

(** {2 DHT} *)

(** The DHT is a Chord network. It uses {!NET} for communications, and
    {!CONFIG} for its parameters.

    @see {{: http://en.wikipedia.org/wiki/Chord_%28peer-to-peer%29 } wikipedia}.
    for a description of Chord. *)

(** Signature of the DHT *)
module type S = sig
  module Net : NET
  module Config : CONFIG
  module Rpc : RPC with module Net = Net

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

  val create : ?log:bool -> ?id:id -> ?payload:string -> Net.t -> t
    (** New DHT, using the given network node. If no ID is provided,
        a new random one is used.
        [payload] is an optional string that is attached to the newly
        created node. *)

  val local : t -> node
    (** Node that represents this very DHT node *)

  val id : node -> id
    (** ID of the given DHT node *)

  val addresses : node -> address list
    (** Address(es) the node can be contacted with. *)

  val payload : node -> string
    (** Payload of a node *)

  val connect : t -> address -> node option Lwt.t
    (** Try to connect to the remote note, returns the remote node
        on success. *)

  val find_node : t -> id -> node option Lwt.t
    (** Returns the successor node of the given ID. It may fail, in
        which case [None] is returned. *)

  val notify : t -> id -> Bencode.t -> unit
    (** Send the given message to the nearest successor of the given ID *)

  (** {2 Register to events} *)

  val messages : t -> Bencode.t Signal.t
    (** Stream of incoming messages *)

  type change_event =
    | Join of node
    | Part of node

  val changes : t -> change_event Signal.t
    (** Changes in the network. Not all join/parts are known to the local node,
        but it is still an interesting information (especially about immediate
        redundancy). *)

    (** {2 Misc} *)

    val enable_log : ?on:out_channel -> t -> unit
      (** Print events related to the DHT on the given channel *)
end

(* TODO: detect when successor dies, in which case update successors list *)

module Make(Net : NET)(Config : CONFIG) = struct
  module Net = Net

  type address = Net.Address.t

  module Config = Config

  module BI = Big_int

  module B = Bencode

  let (>>=) = Lwt.(>>=)

  module Rpc = MakeRPC(Net)

  let n = 160
    (** Number of bits in the ID space *)

  type id = string
    (** A string that uniquely identifies a node on the DHT *)

  type node = {
    n_id : BI.big_int;
    mutable n_addresses : address list;
    mutable n_payload : string;
    mutable n_timeout : float;  (* time at each the node is considered dead *)
  } (** A node of the DHT *)

  (* weak table of nodes *)
  module W = Weak.Make(struct
    type t = node
    let equal n1 n2 = n1.n_id = n2.n_id
    let hash n = Hashtbl.hash n.n_id
  end)

  type t = {
    local : node;
    nodes : W.t; (* weak table of nodes *)
    rpc : Rpc.t;
    logs : string Signal.t;
    mutable predecessor : node option;
    mutable successors : node list;  (* a list of the {! Config.redundancy} successors *)
    fingers : node option array; (* accelerate lookups *)
    mutable next_stabilize : float;
    mutable next_fingers : float;
    mutable finger_to_fix : int; (* index of finger to fix *)
    messages : Bencode.t Signal.t;
    changes : change_event Signal.t;
  } (** The local node *)

  and change_event =
    | Join of node
    | Part of node

  let _addr_to_str addr =
    Bencode.pretty_to_str (Net.Address.encode addr)

  let _start_time = Unix.gettimeofday ()

  (* print a log message *)
  let _log ~dht format =
    let b = Buffer.create 15 in
    Printf.bprintf b "[dht %5s at %.2f] "
      (BI.string_of_big_int dht.local.n_id) (Unix.gettimeofday () -. _start_time);
    Printf.kbprintf
      (fun b ->
        Buffer.add_char b '\n';
        let s = Buffer.contents b in
        Signal.send dht.logs s)
      b format

  let enable_log ?(on=stderr) dht =
    Signal.on dht.logs (fun msg -> output_string on msg; true)

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

  let node_to_bencode node =
    B.L [B.S (string_of_id node.n_id);
         B.L (List.map Net.Address.encode node.n_addresses);
         B.S node.n_payload]

  (* get the unique node object for this ID *)
  let _get_node ~dht id =
    let node = {
      n_id = id;
      n_addresses = [];
      n_payload = "";
      n_timeout = Unix.gettimeofday () +. Config.node_timeout;
    } in
    W.merge dht.nodes node

  (* add address to the list of addresses of this node *)
  let _add_address_to node addr =
    if not (List.exists (fun a' -> Net.Address.eq a' addr) node.n_addresses)
      then node.n_addresses <- addr :: node.n_addresses

  (* increase the time to live of this node, because we just saw it *)
  let _touch_node node =
    let now = Unix.gettimeofday () in
    node.n_timeout <- now +. Config.node_timeout

  (* pick an address among the addresses of the node *)
  let _pick_addr node =
    match node.n_addresses with
    | [] -> raise Not_found
    | a::_ -> a

  let node_of_bencode ~dht msg =
    match msg with
    | B.L [B.S id; B.L addresses; B.S payload] ->
      let node = _get_node ~dht (id_of_string id) in
      let addresses = List.map Net.Address.decode addresses in
      List.iter (_add_address_to node) addresses;
      node
    | _ -> invalid_arg "Chord.node_of_bencode: invalid B-encoded message"

  let _notify_join ~dht node =
    Signal.send dht.changes (Join node)

  let _notify_part ~dht node =
    Signal.send dht.changes (Part node)

  (* immediate successor *)
  let _get_successor ~dht =
    match dht.successors with
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

  let rec _list_take n = function
    | _ when n = 0 -> []
    | [] -> []
    | x::l' -> x :: _list_take (n-1) l'

  let _check_if_successor ~dht node =
    match dht.successors with
    | [] ->
      dht.successors <- [node];
      _log ~dht "successor is now %s" (string_of_id node.n_id);
    | successor::_ ->
      if _between node.n_id
        (BI.succ_big_int dht.local.n_id)
        (BI.pred_big_int successor.n_id)
        then begin
          (* found a closer successor *)
          dht.successors <- _list_take Config.redundancy (node :: dht.successors);
          _log ~dht "successor is now %s" (string_of_id successor.n_id);
        end

  (** The wire format is B-encoded messages. At the top-level, the B-encode
      structures are composed of a constructor (indicating which method
      is called) or "reply", an optional "tag" for query/reply, and arguments.
      
      find the node that is the nearest successor of id
      ---> find_node: ["find", id]
      <--- found:     [node]
      
      ping a remote node to check it's alive and reachable
      ---> ping: ["ping"]
      <--- pong: []

      query a node for its predecessor
      ---> predecessor: ["predecessor"]
      <---              [node]

      notify a node that another node exists
      ---> consider:  ["consider"; node]

      user-level message
      ---> message: ["msg"; bencoded value]

      introduce oneself to another node; the former replies with
      its information (id, addresses, etc.) and tells the sender
      which address it seems to have
      ---> hello:   ["hello", node of sender]
      <---          [address of sender; node of receiver]
      *)

  (* some (possibly remote) node wants to know the successor of [id], and
    calls the callback [k] with the answer. *)
  let _find_successor ~dht id k =
    if id = dht.local.n_id
      then k (Some dht.local)  (* it's me! *)
    else if _between id dht.local.n_id (_get_successor dht).n_id
      then k (Some (_get_successor ~dht))  (* known successor *)
    else
      (* ask the highest preceding node *)
      let n' = _closest_preceding_node ~dht id in
      let addr = _pick_addr n' in
      let msg = B.L [ B.S "find"; B.S (string_of_id id) ] in
      let fut = Rpc.send dht.rpc ~timeout:Config.timeout addr msg in
      Lwt.on_success fut
        (function
        | None -> k None
        | Some node ->
          try
            let node = node_of_bencode ~dht node in
            k (Some node)
          with Invalid_argument _ ->
            k None)

  (* ping the given address to see if it answers. The answer is given to
      the callback [k] *)
  let _ping ~dht addr k =
    let msg = B.S "ping" in
    _log ~dht "ping %s" (Bencode.to_string (Net.Address.encode addr));
    let fut = Rpc.send dht.rpc ~timeout:Config.node_timeout addr msg in
    Lwt.on_success fut
      (function
        | Some (B.S "pong") ->
          _log ~dht "pong from %s" (_addr_to_str addr);
          k true
        | _ ->
          k false)

  let _pong ~dht tag =
    let msg = B.S "pong" in
    Rpc.reply dht.rpc tag msg

  (* what is youuuuur favorite colour^W predecessor? *)
  let _ask_predecessor ~dht addr k =
    let msg = B.S "predecessor" in
    let fut = Rpc.send dht.rpc ~timeout:Config.timeout addr msg in
    Lwt.on_success fut
      (function
        | Some node ->
          begin try
            let node = node_of_bencode ~dht node in
            k (Some node)
          with _ -> k None
          end
        | _ -> k None)

  (* send my predecessor to [addr] *)
  let _reply_predecessor ~dht tag =
    match dht.predecessor with
    | None -> ()  (* do not reply :p *)
    | Some node ->
      try
        let msg = node_to_bencode node in
        Rpc.reply dht.rpc tag msg
      with Invalid_argument _ ->
        ()

  (* notify the given remote node that we think it's our successor *)
  let _consider ~dht addr =
    let msg = B.L [B.S "consider"; node_to_bencode dht.local] in
    Rpc.notify dht.rpc addr msg;
    ()

  (* send a user-defined message to the given node *)
  let _send_user_message ~dht id msg =
    (* first we need the address of the successor node *)
    _find_successor ~dht id
      (function
      | None -> ()  (* could not find ID *)
      | Some node ->
        (* found the recipient of the message, now send it the message *)
        try
          let msg' = B.L [B.S "msg"; msg] in
          Rpc.notify dht.rpc (_pick_addr node) msg'
        with _ -> ())

  (* we received a "consider" message from the given node *)
  let _handle_consider ~dht node =
    match dht.predecessor with
    | None ->
      dht.predecessor <- Some node  (* update predecessor *)
    | Some node'
      when _between node.n_id
        (BI.succ_big_int node'.n_id) (BI.pred_big_int dht.local.n_id) ->
      dht.predecessor <- Some node  (* update predecessor *)
    | _ -> ()

  (* reply to a "hello" message *)
  let _handle_hello ~dht sender_addr sender_node tag =
    let msg = B.L [ Net.Address.encode sender_addr; node_to_bencode dht.local ] in
    Rpc.reply dht.rpc tag msg;
    _check_if_successor ~dht sender_node;
    ()

  (* handle incoming messages *)
  let _dispatch_msg ~dht (sender, tag, msg) =
    begin try
    begin match msg, tag with
    | B.L [B.S "find"; B.S dest_id], Some tag ->
      _log ~dht "%s find_node %s\n" (_addr_to_str sender) dest_id;
      (* ask for the node that is the immediate successor of [dest_id] *)
      let dest_id = id_of_string dest_id in
      _find_successor ~dht dest_id
        (function
        | Some node ->
          (* got a reply, forward it *)
          let msg' = B.L [B.S "found"; node_to_bencode node] in
          Rpc.reply dht.rpc tag msg'
        | None -> ())
    | B.S "ping", Some tag -> (* must reply to the "ping" *)
      _pong ~dht tag
    | B.L [B.S "consider"; node], None ->
      let node = node_of_bencode ~dht node in
      _log ~dht "%s said: consider %s\n" (_addr_to_str sender) (string_of_id node.n_id);
      _handle_consider ~dht node
    | B.S "predecessor", Some tag ->
      _log ~dht "%s asked my predecessor\n" (_addr_to_str sender);
      _reply_predecessor ~dht tag
    | B.L [B.S "msg"; msg], _ -> (* deliver message *)
      _log ~dht "msg %s from %s\n" (Bencode.pretty_to_str msg) (_addr_to_str sender);
      Signal.send dht.messages msg
    | B.L [B.S "hello"; sender_node], Some tag -> (* reply to hello *)
      let sender_node = node_of_bencode ~dht sender_node in
      _log ~dht "hello from %s\n" (string_of_id sender_node.n_id);
      _touch_node sender_node;
      _handle_hello ~dht sender sender_node tag
    | _ -> ()
    end
    with e ->
      Printf.printf "exception %s in _dispatch_msg\n" (Printexc.to_string e);
      Printexc.print_backtrace stdout;
    end;
    true

  (* check whether the predecessor is alive *)
  let _check_predecessor ~dht =
    match dht.predecessor with
    | None -> ()
    | Some node ->
      let k = function
        | false ->
          dht.predecessor <- None;  (* predecessor died *)
          _notify_part ~dht node
        | true ->
          _touch_node node
      in
      (* ping the predecessor *)
      let addr = _pick_addr node in
      _ping ~dht addr k

  (* remove from the list of successors nodes that timeouted *)
  let _clean_successors ~dht =
    let now = Unix.gettimeofday () in
    let dead, alive = List.partition (fun n -> n.n_timeout < now) dht.successors in
    dht.successors <- alive;
    List.iter (fun n -> Signal.send dht.changes (Part n)) dead;
    ()

  (* check whether new successors have joined *)
  let _stabilize ~dht =
    _clean_successors ~dht;
    let successor = _get_successor ~dht in
    _log ~dht "stabilize (current successor %s)" (string_of_id successor.n_id);
    try
      let addr = _pick_addr successor in
      _ask_predecessor ~dht addr
        (fun msg ->
          begin match msg with
          | None -> ()
          | Some node ->
            _check_if_successor ~dht node
          end;
          (* notify the successor of our presence *)
          let addr = _pick_addr (_get_successor ~dht) in
          _consider ~dht addr)
    with _ -> ()

  (* update the given finger *)
  let _fix_finger ~dht n =
    (* find a new finger *)
    let find_finger () =
      let id = _plus_power_two dht.local.n_id n in
      _find_successor ~dht id
        (function
          | None -> ()   (* no finger *)
          | Some node -> (* new finger found *)
            dht.fingers.(n) <- Some node)
    in
    (* check whether we have a valid finger *)
    match dht.fingers.(n) with
    | None -> find_finger ()
    | Some node ->
      let addr = _pick_addr node in
      _ping ~dht addr
        (function
          | false ->
            dht.fingers.(n) <- None;
            find_finger ()  (* finger died, find another one *)
          | true ->
            _touch_node node (* finger ok *)
          )

  (* update the fingers table *)
  let _fix_fingers ~dht =
    dht.finger_to_fix <- dht.finger_to_fix + 1;
    (if dht.finger_to_fix = n
      then dht.finger_to_fix <- 1);
    _fix_finger ~dht dht.finger_to_fix

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

  (* TODO: run a Lwt thread that will wait until next things to do;
     maybe, write a scheduler for this... or use Net.sleep every time we
     schedule a future task *)

  (* create a new DHT node *)
  let create ?(log=false) ?id ?(payload="") net =
    let id = match id with
      | Some i -> BI.big_int_of_string i
      | None -> _random_id ()
    in
    (* local node *)
    let local = {
      n_id = id;
      n_addresses = [];
      n_payload = payload;
      n_timeout = infinity;
    } in
    let dht = {
      local;
      rpc = Rpc.create ~frequency:1. net;
      nodes = W.create 256;
      logs = Signal.create ();
      predecessor=None;
      successors=[];
      fingers = Array.create n None;
      next_stabilize = infinity;
      next_fingers = infinity;
      finger_to_fix = 0;
      messages = Signal.create ();
      changes = Signal.create ();
    } in
    (* listen for incoming RPC messages *)
    Signal.on (Rpc.received dht.rpc) (_dispatch_msg ~dht);
    (* log *)
    (if log then enable_log dht);
    _log ~dht "created";
    dht

  (* Get the ID of the DHT. Returns a copy, to be sure that
    it's not modified *)
  let local dht =
    dht.local

  let random_id () =
    string_of_id (_random_id ())

  let id node = string_of_id node.n_id

  let addresses node = node.n_addresses

  let payload node = node.n_payload

  let connect dht addr =
    let msg = B.L [ B.S "hello"; node_to_bencode dht.local ] in
    let fut = Rpc.send dht.rpc ~timeout:(Config.timeout *. 3.) addr msg in
    (* answer future *)
    let future, promise = Lwt.wait () in
    Lwt.on_success fut
      (function
        | Some (B.L [ my_addr; node ]) ->
          begin try
            (* update my list of addresses *)
            let my_addr = Net.Address.decode my_addr in
            _add_address_to dht.local my_addr;
            (* update successor with fresh node *)
            let node = node_of_bencode ~dht node in
            _check_if_successor ~dht node;
            _consider ~dht addr;
            (* ready updates of the topology very soon *)
            let now = Unix.gettimeofday () in
            dht.next_stabilize <- now +. 1.;
            dht.next_fingers <- now +. 2.;
            (* return the node's ID *)
            Lwt.wakeup promise (Some node)
          with _ -> Lwt.wakeup promise None
          end
        | _ ->
          Lwt.wakeup promise None);
    future

  let find_node dht id =
    let id = id_of_string id in
    let future, promise = Lwt.wait () in
    _find_successor ~dht id (Lwt.wakeup promise);
    future

  let notify dht id msg =
    let id = id_of_string id in
    _send_user_message ~dht id msg

  let messages dht =
    dht.messages

  let changes dht =
    dht.changes
end
