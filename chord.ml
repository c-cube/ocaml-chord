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

module B = Bencode

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
    | Stop  (* stop the DHT *)

  val events : t -> event Signal.t
    (** Signal transmitting events that occur on the network *)

  val call_in : float -> (unit -> unit) -> unit
    (** Call the function in the given amount of seconds *)
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
    if rpc.stop
      then ()
      else
        Net.call_in rpc.frequency
          (fun () ->
            check_timeouts rpc;
            poll rpc)

  let stop rpc =
    rpc.stop <- true

  (* handle network event *)
  let handle_event rpc ev = match ev with
    | Net.Stop -> stop rpc; false
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
    poll rpc;
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
  val dimension : int
    (** Logarithm (base 2) of the size of the ID space. Default is 160. *)

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
  let dimension = 160

  let redundancy = 5

  let stabilize_frequency = 5.

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

  (** Unique identifier for a node *)
  module ID : sig
    type t

    val of_string : string -> t
      (** Parses an ID from a string, or @raise Invalid_argument *)

    val to_string : t -> string

    val eq : t -> t -> bool

    val hash : t -> int
  end

  type address = Net.Address.t

  type t
    (** Instance of the DHT. This contains the state (routing table,
        fingers, etc.) of a node of the DHT. *)

  type node
   (** The representation of a node of the DHT. It relates an ID (a hash
        uniquely identifying the remote node in the DHT) to a network address
        and a node payload (private key, owner metadata, etc.) *)

  val random_id : unit -> ID.t
    (** A fresh, unique ID usable on the network *)

  val create : ?log:bool -> ?id:ID.t -> ?payload:string -> Net.t -> t
    (** New DHT, using the given network node. If no ID is provided,
        a new random one is used.
        [payload] is an optional string that is attached to the newly
        created node. *)

  val local : t -> node
    (** Node that represents this very DHT node *)

  val rpc : t -> Rpc.t
    (** RPC instance *)

  val id : node -> ID.t
    (** ID of the given DHT node *)

  val addresses : node -> address list
    (** Address(es) the node can be contacted with. *)

  val payload : node -> string
    (** Payload of a node *)

  val connect : t -> address -> node option Lwt.t
    (** Try to connect to the remote note, returns the remote node
        on success. *)

  val find_node : t -> ID.t -> node option Lwt.t
    (** Returns the successor node of the given ID. It may fail, in
        which case [None] is returned. *)

  val successor : t -> node
    (** Current successor of this node *)

  val successors : t -> int -> node list
    (** Find the [k] successors of this node *)

  (** {2 Register to events} *)

  type change_event =
    | NewNode of node
    | Timeout of node

  val changes : t -> change_event Signal.t
    (** Changes in the network. Not all join/parts are known to the local node,
        but it is still an interesting information (especially about immediate
        redundancy). *)

  (** {2 Mixtbl store for adding features to the DHT} *)

  module Mixtbl : sig
    val table : t -> string Mixtbl.t
      (** Raw access to the mixtbl *)

    val access : unit -> (string,'a) Mixtbl.injection
      (** New accessor for some type 'a *)

    val set : inj:(string,'a) Mixtbl.injection -> t -> string -> 'a -> unit
      (** Add a key/value pair *)

    val get : inj:(string,'a) Mixtbl.injection -> t -> string -> 'a
      (** Retrieve the value for this given key, or @raise Not_found *)
  end

  (** {2 Overlay network} *)

  module OverlayNet : NET with type Address.t = ID.t and type t = t
    (** See the DHT as a networking device *)

  module AsRPC : RPC with module Net = OverlayNet
    (** Remote procedure calls on the overlay network *)

  (** {2 Misc} *)

  val enable_log : ?on:out_channel -> t -> unit
    (** Print events related to the DHT on the given channel *)

  val wait : t -> unit Lwt.t
    (** Wait for the DHT to stop *)

  val stop : t -> unit
    (** Stop the DHT *)

  val stopped : t -> bool
    (** Is the DHT already stopped? *)

  val log : t -> ('a, Buffer.t, unit, unit) format4 -> 'a
    (** Log a message *)
end

module Make(Net : NET)(Config : CONFIG) = struct
  module Net = Net

  type address = Net.Address.t

  module Config = Config

  module BI = Big_int

  let (>>=) = Lwt.(>>=)

  let n = Config.dimension
    (** Number of bits in the ID space *)

  (** A list of addresses, sorted by decreasing last time we saw
      this address *)
  module AddressList = struct
    type t = address list ref

    let create () = ref []

    let _eq = Net.Address.eq

    (* is the address present? *)
    let mem l addr =
      List.exists (fun a' -> _eq addr a') !l

    (* add/refresh the given address *)
    let touch l addr =
      if mem l addr
        then l := addr :: (List.filter (fun x -> not (_eq x addr)) !l)
        else l := addr :: !l

    (* merge the second list into the first one *)
    let merge ~into l2 =
      List.iter (touch into) !l2

    (* pick an address in the list, or @raise Not_found *)
    let pick l = match !l with
      | [] -> raise Not_found
      | a::_ -> a

    (* iter on addresses *)
    let iter l f =
      List.iter f !l

    let to_list l = !l

    let encode l =
      B.L (List.map Net.Address.encode !l)

    let decode = function
      | B.L l ->
        begin try
          let l = List.map Net.Address.decode l in
          ref l
        with _ ->
          raise (Invalid_argument "could not decode address list")
        end
      | _ ->
        raise (Invalid_argument "could not decode address list")

    (* print the list *)
    let fmt fmt l =
      Format.fprintf fmt "@[<h>";
      List.iteri
        (fun i a ->
          (if i > 0 then Format.fprintf fmt " ");
          Bencode.pretty fmt (Net.Address.encode a))
        !l;
      Format.fprintf fmt "@]"
  end

  module ID = struct
    type t = BI.big_int

    let of_string = BI.big_int_of_string

    let to_string = BI.string_of_big_int

    let eq = BI.eq_big_int

    let compare = BI.compare_big_int

    let hash i = Hashtbl.hash (BI.string_of_big_int i)
  end

  (** Manage the ring structure of nodes *)
  module Ring = struct
    let () =
      if n < 2 then failwith "Chord.Ring: dimension <= 2 is too low"

    module IDMap = Map.Make(struct
      type t = ID.t
      let compare = ID.compare
    end)

    type node = {
      n_id : ID.t;
      n_addresses : AddressList.t;
      mutable n_payload : string;
      mutable n_timeout : float;  (* time at which the node is considered dead *)
      mutable n_confirmed : bool; (* do we know this node exists? *)
    } (** A node of the DHT *)

    type t = {
      local : node;
      mutable nodes : node IDMap.t;
      on_timeout : node Signal.t;   (* nodes that timed out *)
    } (** Ring structure *)

    (* create a new ring *)
    let create local =
      let ring = {
        local = local;
        nodes = IDMap.empty;
        on_timeout = Signal.create ();
      } in
      ring

    let _bound = BI.power_big_int_positive_int (BI.big_int_of_int 2) n

    let _modulo i = BI.mod_big_int i _bound

    let _succ i = _modulo (BI.succ_big_int i)

    let _pred i = _modulo (BI.pred_big_int i)

    (* [_plus_power_two i j] = [i + 2 ** j] *)
    let _plus_power_two i j =
      let two_power_j = BI.power_big_int_positive_int (BI.big_int_of_int 2) j in
      _modulo (BI.add_big_int i two_power_j)

    (* [between i j k] is true iff [i] is between [j] and [k] on the ring *)
    let _between i j k =
      if BI.le_big_int j k
        then (BI.le_big_int j i && BI.le_big_int i k)
        else (BI.le_big_int j i || BI.le_big_int i k)

    (* same as [_between], but excluding the bounds *)
    let _between_strict i j k =
      if BI.le_big_int j k
        then (BI.lt_big_int j i && BI.lt_big_int i k)
        else (BI.lt_big_int j i || BI.lt_big_int i k)

    (* [n] is within [[left ... right]] *)
    let between n left right =
      _between n.n_id left.n_id right.n_id

    (* [n] is within [\]left ... right\[] *)
    let between_strict n left right =
      _between_strict n.n_id left.n_id right.n_id

    (* get node by ID *)
    let get ring id =
      try IDMap.find id ring.nodes
      with Not_found ->
        let node = {
          n_id = id;
          n_addresses = AddressList.create ();
          n_payload = "";
          n_timeout = Unix.gettimeofday () +. Config.node_timeout;
          n_confirmed = false;
        } in
        ring.nodes <- IDMap.add node.n_id node ring.nodes;
        node

    (* local node *)
    let local ring = ring.local

    (* first node with id >= id *)
    let _find ring id =
      match IDMap.split id ring.nodes with
      | _, Some n, _ -> n  (* some node has this exact ID *)
      | l, None, r -> 
        try
          snd (IDMap.min_binding r)
        with Not_found ->
          try snd (IDMap.min_binding l)  (* wrap *)
          with Not_found ->
            ring.local

    (* current successor *)
    let successor ring =
      _find ring (BI.succ_big_int ring.local.n_id)

    (* n successors *)
    let n_successors ring k =
      let rec find ring id k =
      if k = 0
        then []
        else
          let n = _find ring id in
          n :: find ring (BI.succ_big_int n.n_id) (k-1)
      in
      find ring (BI.succ_big_int ring.local.n_id) k

    (* the known node that immediately precedes [id] *)
    let _closest_preceding_node ring id =
      match IDMap.split id ring.nodes with
      | l, _, r -> 
        try
          snd (IDMap.max_binding l)
        with Not_found ->
          try snd (IDMap.max_binding r)  (* wrap *)
          with Not_found ->
            ring.local (* no other known node *)

    (* current predecessor *)
    let predecessor ring =
      _closest_preceding_node ring ring.local.n_id

    (* [k]-th finger *)
    let finger ring k =
      let id = _plus_power_two ring.local.n_id k in
      _find ring id

    (* remove the node explicitely *)
    let remove ring id =
      ring.nodes <- IDMap.remove id ring.nodes

    (* number of known nodes in the ring *)
    let size ring =
      IDMap.cardinal ring.nodes

    (* we just saw this node (from the given address) *)
    let touch ring ~from ~node =
      let now = Unix.gettimeofday () in
      node.n_timeout <- now +. Config.node_timeout;
      node.n_confirmed <- true;
      AddressList.touch node.n_addresses from;
      ()
      
    (* check if the node timeouted *)
    let check_timeout ring id =
      let now = Unix.gettimeofday () in
      try
        let node = IDMap.find id ring.nodes in
        (* local node cannot time out *)
        if node != ring.local && node.n_timeout < now
          then begin  (* remove this node, trigger signal *)
            remove ring id;
            Signal.send ring.on_timeout node;
          end
      with Not_found ->
        ()

    (* nodes that timed out *)
    let on_timeout ring = ring.on_timeout

    let node_to_bencode node =
      B.L [B.S (ID.to_string node.n_id);
           AddressList.encode node.n_addresses;
           B.S node.n_payload]

    let node_of_bencode ring msg =
      match msg with
      | B.L [B.S id; addresses; B.S payload] ->
        let node = get ring (ID.of_string id) in
        let addresses = AddressList.decode addresses in
        AddressList.merge ~into:node.n_addresses addresses;
        node
      | _ ->
        invalid_arg "Chord.node_of_bencode: invalid B-encoded message"
  end

  type node = Ring.node

  module Rpc = MakeRPC(Net)

  type t = {
    local : Ring.node;
    ring : Ring.t;
    rpc : Rpc.t;
    logs : string Signal.t;
    mutable finger_to_fix : int; (* index of finger to fix *)
    messages : (ID.t * Bencode.t) Signal.t;
    changes : change_event Signal.t;
    do_stop : unit Lwt.u;
    on_stop : unit Lwt.t;
    mixtbl : string Mixtbl.t;   (* extensible table *)
  } (** The local node *)
  and change_event =
    | NewNode of node
    | Timeout of node

  (* is the DHT stopped? *)
  let stopped dht =
    match Lwt.state dht.on_stop with
    | Lwt.Sleep -> false
    | _ -> true

  let _addr_to_str addr =
    Bencode.pretty_to_str (Net.Address.encode addr)

  let _start_time = Unix.gettimeofday ()

  (* print a log message *)
  let _log ~dht format =
    let b = Buffer.create 15 in
    Printf.bprintf b "[dht %5s at %.2f] "
      (ID.to_string dht.ring.Ring.local.Ring.n_id) (Unix.gettimeofday () -. _start_time);
    Printf.kbprintf
      (fun b ->
        Buffer.add_char b '\n';
        let s = Buffer.contents b in
        Signal.send dht.logs s)
      b format

  let enable_log ?(on=stderr) dht =
    Signal.on dht.logs (fun msg -> output_string on msg; true)

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
      ---> message: ["msg"; sender ID; bencoded message]

      introduce oneself to another node; the former replies with
      its information (id, addresses, etc.) and tells the sender
      which address it seems to have
      ---> hello:   ["hello", node of sender]
      <---          [address of sender; node of receiver]
      *)

  (* some (possibly remote) node wants to know the successor of [id], and
    calls the callback [k] with the answer. *)
  let _find_successor ~dht id k =
    if ID.eq id dht.local.Ring.n_id
      then k (Some dht.local)  (* it's me! *)
    else if Ring._between id dht.local.Ring.n_id (Ring.successor dht.ring).Ring.n_id
      then k (Some (Ring.successor dht.ring))  (* known successor *)
    else
      (* ask the highest preceding node *)
      let n' = Ring._closest_preceding_node dht.ring id in
      let addr = AddressList.pick n'.Ring.n_addresses in
      let msg = B.L [ B.S "find"; B.S (ID.to_string id) ] in
      let fut = Rpc.send dht.rpc ~timeout:Config.timeout addr msg in
      Lwt.on_success fut
        (function
        | None -> k None
        | Some node ->
          try
            let node = Ring.node_of_bencode dht.ring node in
            k (Some node)
          with Invalid_argument _ ->
            k None)

  (* ping the given address to see if it answers. The answer is given to
      the callback [k] *)
  let _ping ~dht addr k =
    let msg = B.S "ping" in
    _log ~dht "ping %s" (_addr_to_str addr);
    let fut = Rpc.send dht.rpc ~timeout:Config.node_timeout addr msg in
    Lwt.on_success fut
      (function
        | Some (B.S "pong") ->
          _log ~dht "pong from %s" (_addr_to_str addr);
          k true
        | _ ->
          k false)

  (* ping the node, to check it's still alive *)
  let _ping_node ~dht node =
    let l = AddressList.to_list node.Ring.n_addresses in
    let fut = Lwt_list.exists_p
      (fun addr ->
        let fut, promise = Lwt.wait () in
        _ping ~dht addr
          (fun alive ->
            if alive then begin
              Ring.touch dht.ring ~from:addr ~node;
              Lwt.wakeup promise true (* success *)
            end
              else Lwt.wakeup promise false);
        fut)
      l
    in
    fut

  (* reply to a "ping" message *)
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
            let node = Ring.node_of_bencode dht.ring node in
            k (Some node)
          with _ -> k None
          end
        | _ -> k None)

  (* send my predecessor to [addr] *)
  let _reply_predecessor ~dht tag =
    let n = Ring.predecessor dht.ring in
    let msg = Ring.node_to_bencode n in
    Rpc.reply dht.rpc tag msg

  (* notify the given remote node that we think it's our successor *)
  let _consider ~dht addr =
    let msg = B.L [B.S "consider"; Ring.node_to_bencode dht.local] in
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
        let msg' = B.L [B.S "msg"; msg] in
        Rpc.notify dht.rpc (AddressList.pick node.Ring.n_addresses) msg')

  (* reply to a "hello" message *)
  let _handle_hello ~dht sender_addr sender_node tag =
    let msg = B.L [ Net.Address.encode sender_addr; Ring.node_to_bencode dht.local ] in
    Rpc.reply dht.rpc tag msg;
    ()

  (* handle incoming messages *)
  let _dispatch_msg ~dht (sender, tag, msg) =
    begin try
    begin match msg, tag with
    | B.L [B.S "find"; B.S dest_id], Some tag ->
      (* ask for the node that is the immediate successor of [dest_id] *)
      let dest_id = ID.of_string dest_id in
      _find_successor ~dht dest_id
        (function
        | Some node ->
          (* got a reply, forward it *)
          let msg' = Ring.node_to_bencode node in
          Rpc.reply dht.rpc tag msg'
        | None -> ())
    | B.S "ping", Some tag -> (* must reply to the "ping" *)
      _pong ~dht tag
    | B.L [B.S "consider"; node], None ->
      let node = Ring.node_of_bencode dht.ring node in
      _log ~dht "%s said: consider %s"
        (_addr_to_str sender) (ID.to_string node.Ring.n_id);
      (* potentially better predecessor, ping it *)
      if Ring.between_strict node (Ring.predecessor dht.ring) dht.local
        then Lwt.ignore_result (_ping_node ~dht node)
    | B.S "predecessor", Some tag ->
      _reply_predecessor ~dht tag
    | B.L [B.S "msg"; B.S sender_id; msg], _ -> (* deliver message *)
      _log ~dht "msg %s from %s" (Bencode.pretty_to_str msg) sender_id;
      let sender_id = ID.of_string sender_id in
      Signal.send dht.messages (sender_id, msg)
    | B.L [B.S "hello"; sender_node], Some tag -> (* reply to hello *)
      let sender_node = Ring.node_of_bencode dht.ring sender_node in
      _log ~dht "hello from %s" (ID.to_string sender_node.Ring.n_id);
      (* touch the node, we know it's alive *)
      Ring.touch dht.ring ~from:sender ~node:sender_node;
      _handle_hello ~dht sender sender_node tag
    | _ -> ()
    end
    with e ->
      Printf.printf "exception %s in _dispatch_msg\n" (Printexc.to_string e);
      Printexc.print_backtrace stdout;
    end;
    (* continue listening to messages iff the DHT is alive *)
    not (stopped dht)

  (* check whether the predecessor is alive *)
  let _check_predecessor ~dht =
    let node = Ring.predecessor dht.ring in
    _ping_node ~dht node

  (* check whether new successors have joined *)
  let rec _stabilize ~dht =
    let n = Ring.successor dht.ring in
    _log ~dht "stabilize (current successor %s)" (ID.to_string n.Ring.n_id);
    begin try
      let addr = AddressList.pick n.Ring.n_addresses in
      _ask_predecessor ~dht addr
        (fun msg ->
          begin match msg with
          | None -> ()
          | Some node ->
            (* ping predecessor of successor *)
            let fut = _ping_node ~dht node in
            Lwt.on_success fut
              (fun success ->
              if success && Ring.between_strict node dht.local n
                then (* new successor is better? stabilize again! *)
                  Net.call_in 0.1 (fun () -> _stabilize ~dht))
          end;
          (* notify the successor of our presence *)
          let addr = AddressList.pick (Ring.successor dht.ring).Ring.n_addresses in
          _consider ~dht addr)
    with _ -> ()
    end;
    (* schedule next stabilize *)
    if not (stopped dht)
      then Net.call_in Config.stabilize_frequency (fun () -> _stabilize ~dht)

  (* update the given finger *)
  let _fix_finger ~dht n =
    let node = Ring.finger dht.ring n in
    _find_successor ~dht node.Ring.n_id
      (function
        | None -> ()   (* no finger *)
        | Some node -> (* new finger may be found *)
          Lwt.ignore_result (_ping_node ~dht node))

  (* update the fingers table *)
  let rec _fix_fingers ~dht =
    dht.finger_to_fix <- dht.finger_to_fix + 1;
    (if dht.finger_to_fix = n
      then dht.finger_to_fix <- 1);
    _fix_finger ~dht dht.finger_to_fix;
    (* schedule next update *)
    if not (stopped dht)
      then Net.call_in Config.stabilize_frequency (fun () -> _fix_fingers ~dht)

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
     maybe, write a scheduler for this... or use Net.call_in every time we
     schedule a future task *)

  (* create a new DHT node *)
  let create ?(log=false) ?id ?(payload="") net =
    let id = match id with
      | Some i -> i
      | None -> _random_id ()
    in
    (* local node *)
    let local =
      let open Ring in {
      n_id = id;
      n_addresses = AddressList.create ();
      n_payload = payload;
      n_timeout = infinity;
      n_confirmed = true;
    } in
    let ring = Ring.create local in
    let on_stop, do_stop = Lwt.wait () in
    let dht = {
      local;
      ring;
      rpc = Rpc.create ~frequency:1. net;
      logs = Signal.create ();
      finger_to_fix = 0;
      messages = Signal.create ();
      changes = Signal.create ();
      do_stop;
      on_stop;
      mixtbl = Mixtbl.create 13;
    } in
    Signal.on
      (Ring.on_timeout dht.ring)
      (fun n -> Signal.send dht.changes (Timeout n); true);
    (* listen for incoming RPC messages *)
    Signal.on (Rpc.received dht.rpc) (_dispatch_msg ~dht);
    (* log *)
    (if log then enable_log dht);
    _log ~dht "created";
    dht

  let local dht =
    dht.local

  let rpc dht = dht.rpc

  let random_id () =
    _random_id ()

  let id node = node.Ring.n_id

  let addresses node =
    AddressList.to_list node.Ring.n_addresses

  let payload node = node.Ring.n_payload

  let connect dht addr =
    let msg = B.L [ B.S "hello"; Ring.node_to_bencode dht.local ] in
    let fut = Rpc.send dht.rpc ~timeout:(Config.timeout *. 3.) addr msg in
    fut >>= 
      function
        | Some (B.L [ my_addr; node ]) ->
          begin try
            (* update my list of addresses *)
            let my_addr = Net.Address.decode my_addr in
            AddressList.touch dht.local.Ring.n_addresses my_addr;
            (* update successor with fresh node *)
            let node = Ring.node_of_bencode dht.ring node in
            _consider ~dht addr;
            (* ready updates of the topology very soon *)
            Net.call_in 0.2 (fun () -> _stabilize ~dht);
            Net.call_in 0.5 (fun () -> _fix_fingers ~dht);
            (* return the node's ID *)
            Lwt.return (Some node)
          with _ -> Lwt.return_none
          end
        | _ ->
          Lwt.return_none

  let find_node dht id =
    let future, promise = Lwt.wait () in
    _find_successor ~dht id (Lwt.wakeup promise);
    future

  let successor dht =
    Ring.successor dht.ring

  let successors dht k =
    Ring.n_successors dht.ring k

  let changes dht =
    dht.changes

  let stop dht =
    if not (stopped dht)
      then begin
        _log ~dht "stop";
        Lwt.wakeup dht.do_stop ();
      end

  let wait dht = dht.on_stop

  let log dht format = _log ~dht format

  (** {2 Mixtbl store for adding features to the DHT} *)

  module Mixtbl = struct
    let table dht = dht.mixtbl

    let access () = Mixtbl.access ()

    let set ~inj dht key value =
      Mixtbl.set ~inj dht.mixtbl key value

    let get ~inj dht key =
      Mixtbl.find ~inj dht.mixtbl key
  end

  (** {2 Overlay network and RPC} *)

  type dht = t
  module OverlayNet = struct
    module Address = struct
      type t = ID.t

      let encode a = B.S (ID.to_string a)

      let decode = function
        | B.S i -> ID.of_string i
        | _ -> raise (Invalid_argument "not an ID")

      let eq = ID.eq
    end

    type t = dht

    let send dht id msg = _send_user_message ~dht id msg

    type event =
      | Receive of Address.t * Bencode.t   (* received message *)
      | Stop  (* stop the DHT *)

    let events dht =
      let s = Signal.create () in
      Signal.on dht.messages
        (fun (sender,m) -> Signal.send s (Receive(sender, m)); true);
      Lwt.on_success dht.on_stop (fun () -> Signal.send s Stop);
      s

    let call_in = Net.call_in
  end

  module AsRPC = MakeRPC(OverlayNet)
end
