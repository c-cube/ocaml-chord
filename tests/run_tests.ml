
open OUnit

let suite =
  "all_tests" >:::
    [ Test_rpc.suite;
    ]

let _ =
  run_test_tt_main suite
