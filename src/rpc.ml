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

(** {6 RPC} *)

module type NET = Net.S

(** This provides a lightweight RPC mechanism on top of a {!Net.S}
    implementation and B-encoded messages. *)

module type S = sig
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

module Make(Net : Net.S) = struct
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
