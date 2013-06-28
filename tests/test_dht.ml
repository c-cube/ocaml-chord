
open OUnit

module B = Bencode
module Dht = Chord.Make(Net_local)(Chord.ConfigDefault)
module Store = Store.Make(Dht)

let log_net = ref false
let log_dht = ref true

let (>>=) = Lwt.(>>=)

let test_dht () =
  let n1 = Net_local.create ~log:!log_net () in
  let n2 = Net_local.create ~log:!log_net () in
  let n3 = Net_local.create ~log:!log_net () in
  let n4 = Net_local.create ~log:!log_net () in
  let n5 = Net_local.create ~log:!log_net () in
  let dht1 = Dht.create ~log:!log_dht ~payload:"dht1" n1 in
  let dht2 = Dht.create ~log:!log_dht ~payload:"dht2" n2 in
  let dht3 = Dht.create ~log:!log_dht ~payload:"dht3" n3 in
  let dht4 = Dht.create ~log:!log_dht ~payload:"dht4" n4 in
  let dht5 = Dht.create ~log:!log_dht ~payload:"dht5" n5 in
  let all_dht = [dht1; dht2; dht3; dht4; dht5] in
  let fut12 = Dht.connect dht1 (Net_local.address_of n2) in
  let fut13 = Dht.connect dht1 (Net_local.address_of n3) in
  let fut23 = Dht.connect dht2 (Net_local.address_of n3) in
  let fut32 = Dht.connect dht3 (Net_local.address_of n2) in
  let fut51 = Dht.connect dht5 (Net_local.address_of n1) in
  let fut41 = Dht.connect dht4 (Net_local.address_of n1) in
  (* connection *)
  begin match Lwt_main.run
    (Lwt.pick
      [Lwt_list.map_s (fun x->x) [fut12; fut13; fut23; fut32; fut51; fut41];
      Lwt_unix.timeout 2.]) with
  | [Some n2; Some n3; Some n3'; Some n2'; Some n1; Some n1'] -> ()
  | l ->
    let n = List.length (List.filter (fun x -> x = None) l) in
    OUnit.assert_failure (Printf.sprintf "should all connect (%d failed)" n);
  end;
  (* stabilize *)
  Lwt_main.run (Lwt_unix.sleep 2.);
  (* build a store *)
  let all_stores = List.map Store.create all_dht in
  let store1, store2 =
    match all_stores with | s1 :: s2 :: _ -> s1, s2 | _ -> assert false
  in
  let key = Dht.ID.of_string "1" in
  Printf.eprintf "store\n";
  let store_ok = Store.store store1 key "foo" in
  Printf.eprintf "get\n";
  let get_ok =
   (Lwt_unix.sleep 0.5 >>= fun () ->
    Store.get store2 key >>= function
    | Some "foo" -> Lwt.return_true
    | _ -> Lwt.return_false)
  in
  Printf.eprintf "test\n";
  begin match Lwt_main.run (Lwt_list.map_s (fun x->x) [store_ok; get_ok]) with
  | [true; true] -> OUnit.assert_bool "store, get ok" true
  | [b1; b2] -> OUnit.assert_bool (Printf.sprintf "store: %B, get: %B" b1 b2) false
  | _ -> assert false
  end;
  (* termination *)
  Lwt_main.run
    (Lwt.join
      [ Lwt_list.iter_p Dht.wait all_dht;
        (Lwt_unix.sleep 2. >>= fun () -> List.iter Dht.stop all_dht; Lwt.return_unit)]);
  ()

let suite =
  "test_dht" >:: test_dht
