open Corosync_tools
open Cfgtool
open Corosync_lib.Corotypes

let ( >>= ) = Result.bind

let test_local_get () =
  let r = local_get () in
  Alcotest.(check bool) "failed local_get" true (Result.is_error r) ;
  Alcotest.(check string)
    "error matches library" "cs_err_library"
    (Result.get_error r |> CsError.to_string)

let test_reload_config () =
  let r = reload_config () in
  Alcotest.(check bool) "failed reload_config" true (Result.is_error r) ;
  Alcotest.(check string)
    "error matches library" "cs_err_library"
    (Result.get_error r |> CsError.to_string)

let test_get_node_addrs () =
  local_get ()
  >>= (fun nodeid ->
        let addrs = get_node_addrs nodeid in
        Alcotest.(check bool)
          "failed get node_addrs" true (Result.is_error addrs) ;
        Alcotest.(check string)
          "error matches library" "cs_err_library"
          (Result.get_error addrs |> CsError.to_string) ;
        Ok ()
      )
  |> ignore

let () =
  let open Alcotest in
  run "Cfgtool"
    [
      ("local_get", [test_case "local get" `Quick test_local_get])
    ; ("reload_config", [test_case "reload config" `Quick test_reload_config])
    ; ("get_node_addrs", [test_case "get node addrs" `Quick test_get_node_addrs])
    ]
