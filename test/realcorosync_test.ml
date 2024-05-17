let () =
  let open Alcotest in
  run "real corosync tests"
    (Test_cfgtool.tests @ Test_cmapctl.tests @ Test_quorumtool.tests)
