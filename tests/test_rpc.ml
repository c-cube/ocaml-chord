
open OUnit

module B = Bencode
module R = Rpc.Make(Net_local)

let test_notify () =
  let n1 = Net_local.create () in
  let n2 = Net_local.create () in
  let r1 = R.create n1 in
  let r2 = R.create n2 in
  let received = ref None in
  let b = B.S "message1" in
  Signal.on (R.received r1) (fun (_, _, b') -> received := Some b'; true);
  (*
  Signal.on (R.received r1)
    (fun (i, _, b') -> Printf.printf "received %s from %d\n" (B.to_string b') i; true);
  *)
  R.notify r2 (Net_local.address_of n1) b;
  OUnit.assert_equal (Some b) !received;
  (* second test *)
  received := None;
  let b2 = B.S "message2" in
  R.notify r2 (Net_local.address_of n1) b2;
  OUnit.assert_equal (Some b2) !received;
  ()

let test_reply () =
  let n1 = Net_local.create () in
  let n2 = Net_local.create () in
  let r1 = R.create n1 in
  let r2 = R.create n2 in
  Signal.on (R.received r1)
    (function
       | (_, Some tag, B.I i) -> R.reply r1 tag (B.I (i+1)); true
       | _ -> true);
  let fut = R.send r2 (Net_local.address_of n1) (B.I 41) in
  match Lwt_main.run (Lwt.pick [fut; Lwt_unix.timeout 3.]) with
  | Some (B.I 42) -> OUnit.assert_bool "success" true
  | _ -> OUnit.assert_failure "should have received message 42"

let suite =
  "test_rpc" >:::
    [ "test_notify" >:: test_notify
    ; "test_reply" >:: test_reply
    ]
