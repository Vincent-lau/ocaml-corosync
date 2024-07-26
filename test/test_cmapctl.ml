open Corosync_tools
open Corosync_lib
open Cmapctl

let ( >>= ) = Result.bind

let test_get () =
  let r = get Cmap.CmapValue.uint32 "totem.version" in
  Alcotest.(check bool) "successful get" true (Result.is_ok r) ;
  r
  >>= (fun version ->
        Alcotest.(check int) "same version" 2 (Unsigned.UInt32.to_int version) ;
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

let test_set () =
  let r = set "totem.cluster" (CmapString "hello") in
  Alcotest.(check bool) "successful set" true (Result.is_ok r) ;
  get Cmap.CmapValue.string "totem.cluster"
  >>= (fun cn ->
        Alcotest.(check string) "get what you set" "hello" cn ;
        Ok ()
      )
  |> ignore

let tests =
  let open Alcotest in
  [
    ( "get"
    , [
        test_case "get totem.version" `Quick test_get
      ; test_case "get nodelist prefix" `Quick test_get_prefix
      ; test_case "set cluster name and get it" `Quick test_set
      ]
    )
  ]
