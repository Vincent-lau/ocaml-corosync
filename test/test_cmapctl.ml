open Corosync_tools
open Cmapctl

let ( >>= ) = Result.bind

let test_get () =
  let r = get "totem.version" in
  Alcotest.(check bool) "successful get" true (Result.is_ok r) ;
  r
  >>= (fun version ->
        Alcotest.(check int) "same version" 2 (int_of_string version) ;
        Ok ()
      )
  |> ignore

let test_get_prefix () =
  let r = get_prefix "nodelist.node" in
  Alcotest.(check bool) "successful get_prefix" true (Result.is_ok r) ;
  r
  >>= (fun rl ->
        Alcotest.(check bool) "nodelist has elements" true (List.length rl > 0) ;
        Ok ()
      )
  |> ignore

let () =
  let open Alcotest in
  run "Cmapctl"
    [
      ( "get"
      , [
          test_case "get totem.version" `Quick test_get
        ; test_case "get nodelist prefix" `Quick test_get_prefix
        ]
      )
    ]
