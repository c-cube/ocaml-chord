
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

(** {1 Simple broadcast} *)

module B = Bencode

let (>>=) = Lwt.(>>=)

module type S = sig
  module N : Net.S

  type t
    (** Broadcasting algorithm state. This accounts for one node
        in the broadcasting network. *)

  val create : ?cache_timeout:float -> N.t -> t
    (** Create a new broadcasting device *)

  val broadcast : t -> Bencode.t -> unit
    (** Broadcast a message *)

  val connect : t -> N.Address.t -> bool Lwt.t
    (** Connect to another node. Returns true iff it succeeds. *)

  val neighbors : t -> N.Address.t list
    (** Current list of neighbors *)

  type event =
    | Receive of Bencode.t

  val events : t -> event Signal.t
    (** messages broadcasted by others *)

  val recv : t -> event Lwt.t
    (** next event *)
end

module MsgHashtbl = Hashtbl.Make(struct
  type t = int * B.t
  let equal (i1,b1) (i2,b2) = i1 = i2 && B.eq b1 b2
  let hash (i,b) = i + 65399 * (B.hash b)
end)

module Make(N : Net.S) = struct
  module N = N

  module R = Rpc.Make(N)

  type event =
    | Receive of Bencode.t

  type t = {
    cache_timeout : float;
    net : N.t;
    rpc : R.t;
    cache : float MsgHashtbl.t;  (* (id,message) -> time received *)
    mutable neighbors : N.Address.t list;
    events : event Signal.t;
  }

  let _add_neighbor t addr =
    if List.exists (N.Address.eq addr) t.neighbors
      then ()
      else t.neighbors <- addr :: t.neighbors

  let _remove_neighbor t addr =
    t.neighbors <- List.filter (fun a' -> not (N.Address.eq addr a')) t.neighbors

  let rec _cleanup t () =
    let now = Unix.gettimeofday () in
    let to_remove =MsgHashtbl.fold
      (fun (i,msg) time acc ->
        if time +. t.cache_timeout < now
          then (i,msg) :: acc   (* too old, remove *)
          else acc)
      t.cache []
    in
    List.iter (fun (i,msg) -> MsgHashtbl.remove t.cache (i,msg)) to_remove;
    (* do it again soon *)
    N.call_in (t.cache_timeout /. 2.) (_cleanup t);
    ()

  let _broadcast ?but t msg =
    let id = Random.int (1 lsl 29) in
    let msg' = B.L [ B.I id; msg ] in
    List.iter
      (fun addr ->
        match but with
        | Some addr' when N.Address.eq addr addr' -> ()  (* ignore *)
        | _ -> R.notify t.rpc addr msg')
      t.neighbors;
    ()

  let _dispatch t =
    Signal.on (R.received t.rpc)
      (fun e ->
        begin match e with
        | addr, Some reply, B.S "hello" ->
          (* connection *)
          R.reply t.rpc reply (B.S "world")
        | addr, _, (B.L [ B.I id; msg' ] as msg) ->
          if MsgHashtbl.mem t.cache (id, msg')
            then ()  (* already delivered *)
            else
              (* deliver, and re-broadcast to other neighbors *)
              let () = Signal.send t.events (Receive msg') in
              _broadcast t ~but:addr msg
        | _ -> ()
        end;
        true)

  let create ?(cache_timeout=60.) net =
    let t = {
      cache_timeout;
      net;
      rpc = R.create net;
      cache = MsgHashtbl.create 128;
      neighbors = [];
      events = Signal.create ();
    } in
    (* handle incoming messages *)
    _dispatch t;
    (* call _cleanup regularly *)
    N.call_in cache_timeout (_cleanup t);
    t

  let broadcast t msg = _broadcast t msg

  let connect t addr =
    let msg = B.S "hello" in
    R.send t.rpc addr msg >>= function
    | Some (B.S "world") ->
      _add_neighbor t addr;
      Lwt.return_true
    | _ ->
      Lwt.return_false

  let neighbors t = t.neighbors

  let events t = t.events

  let recv t =
    let fut, wake = Lwt.wait () in
    Signal.on
      t.events
      (fun e -> Lwt.wakeup wake e; false);
    fut
end
