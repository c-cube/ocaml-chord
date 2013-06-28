
open OUnit

module B = Bencode
module Dht = Chord.Make(Net_local)(Chord.ConfigDefault)

let log_net = false
let log_dht = true

let test_dht () =
  let n1 = Net_local.create ~log:log_net () in
  let n2 = Net_local.create ~log:log_net () in
  let n3 = Net_local.create ~log:log_net () in
  let n4 = Net_local.create ~log:log_net () in
  let n5 = Net_local.create ~log:log_net () in
  let dht1 = Dht.create ~log:log_dht ~payload:"dht1" n1 in
  let dht2 = Dht.create ~log:log_dht ~payload:"dht2" n2 in
  let dht3 = Dht.create ~log:log_dht ~payload:"dht3" n3 in
  let dht4 = Dht.create ~log:log_dht ~payload:"dht4" n4 in
  let dht5 = Dht.create ~log:log_dht ~payload:"dht5" n5 in
  let fut12 = Dht.connect dht1 (Net_local.address_of n2) in
  let fut13 = Dht.connect dht1 (Net_local.address_of n3) in
  let fut23 = Dht.connect dht2 (Net_local.address_of n3) in
  let fut32 = Dht.connect dht3 (Net_local.address_of n2) in
  let fut51 = Dht.connect dht5 (Net_local.address_of n1) in
  let fut41 = Dht.connect dht4 (Net_local.address_of n1) in
  begin match Lwt_main.run
    (Lwt.pick
      [Lwt_list.map_s (fun x->x) [fut12; fut13; fut23; fut32; fut51; fut41];
      Lwt_unix.timeout 2.]) with
  | [Some n2; Some n3; Some n3'; Some n2'; Some n1; Some n1'] -> ()
  | l ->
    let n = List.length (List.filter (fun x -> x = None) l) in
    OUnit.assert_failure (Printf.sprintf "should all connect (%d failed)" n);
  end;
  ()

let suite =
  "test_dht" >:: test_dht
