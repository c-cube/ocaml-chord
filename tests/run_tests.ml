
open OUnit

let suite =
  "all_tests" >:::
    [ Test_rpc.suite;
      Test_dht.suite;
    ]

let options =
  [ "-lognet", Arg.Set Test_dht.log_net, "enable network logs"
  ; "-logdht", Arg.Bool (fun b -> Test_dht.log_dht := b), "enable/disable DHT logs"
  ]

let _ =
  run_test_tt_main ~arg_specs:options suite
