open Corosync.Cfgtool

let ( >>= ) = Result.bind

let test_local_get () =
  let r = local_get () in
  Alcotest.(check bool) "successful local_get" true (Result.is_ok r)

let test_reload_config () =
  let r = reload_config () in
  Alcotest.(check bool) "successful reload_config" true (Result.is_ok r)

let test_get_node_addrs () =
  local_get ()
  >>= (fun nodeid ->
        let addrs = get_node_addrs nodeid in
        Alcotest.(check bool)
          "successful get node_addrs" true (Result.is_ok addrs) ;
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
