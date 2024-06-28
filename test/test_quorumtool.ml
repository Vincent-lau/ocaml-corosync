open Corosync_tools
open Quorumtool

let ( >>= ) = Result.bind

let test_is_quorate () =
  let r = is_quorate () in
  ( Alcotest.(check bool) "successful is_quorate" true (Result.is_ok r) ;
    r >>= fun quorate ->
    Alcotest.(check bool) "is quorate" true quorate ;
    Ok ()
  )
  |> ignore

let test_using_vote_quorum () =
  let r = using_votequorum () in
  Alcotest.(check bool) "is using vote quorum" true r

let test_votequorum_info () =
  let r = votequorum_info () in
  Alcotest.(check bool) "successful votequorum info" true (Result.is_ok r)

let tests =
  let open Alcotest in
  [
    ("quorum", [test_case "is quorate" `Quick test_is_quorate])
  ; ( "votequorum"
    , [
        test_case "is using votequorum" `Quick test_using_vote_quorum
      ; test_case "get node addrs" `Quick test_votequorum_info
      ]
    )
  ]
