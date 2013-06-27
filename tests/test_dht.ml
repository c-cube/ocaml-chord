
open OUnit

module B = Bencode
module Dht = Chord.Make(Net_local)(Chord.ConfigDefault)

let test_dht () =
  let n1 = Net_local.create ~log:true () in
  let n2 = Net_local.create ~log:true () in
  let dht1 = Dht.create ~payload:"dht1" n1 in
  let dht2 = Dht.create ~payload:"dht2" n2 in
  let fut = Dht.connect dht1 (Net_local.address_of n2) in
  begin match Lwt_main.run (Lwt.pick [fut; Lwt_unix.timeout 2.]) with
  | None -> OUnit.assert_failure "should connect";
  | Some n2 -> ()
  end;
  ()

let suite =
  "test_dht" >:: test_dht
