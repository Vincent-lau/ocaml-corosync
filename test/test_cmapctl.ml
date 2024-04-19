open Cmapctl

let test_get () =
  get "totem.version" >>= fun version ->
  Alcotest.(check bool) "got result" true (Result.is_ok version) ;
  Alcotest.(check int) "same version" 2 (Result.get_ok version)
