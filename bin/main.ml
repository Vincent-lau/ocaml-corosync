open Corosync
open Corotypes
open CsError
open Quorumtool

let _test_cmap () =
  let open Cmap in
  ( with_handle @@ fun handle ->
    Cmap.get handle "nodelist.node.0.name" |> function
    | Ok l ->
        print_endline l ;
        (* List.iter
           (fun (k, v) -> Printf.printf "%s: %s\n" k v)
           l ; *)
        Ok ()
    | Error e ->
        failwith (to_string e)
  )
  |> ignore

let _test_quorum () =
  let open Quorum in
  ( with_handle @@ fun handle ->
    match getquorate handle with
    | Ok b ->
        Printf.printf "is quorate %b\n" b ;
        print_int (ViewList.get_view_list_entries ()) ;
        print_newline () ;
        List.iter
          (fun n ->
            Printf.printf "nodeid %d node name %s"
              ViewList.(n.node_id)
              ViewList.(Option.get n.name) ;
            print_string " "
          )
          (ViewList.get_view_list ()) ;
        print_newline () ;
        Ok ()
    | Error e ->
        failwith (to_string e)
  )
  |> ignore

let _test_votequorum () =
  let open Votequorum in
  ( with_handle @@ fun handle ->
    match getinfo handle with
    | Ok info ->
        Printf.printf "total votes %d\n" (Unsigned.UInt.to_int info.total_votes) ;
        Printf.printf "quorum %d\n" (Unsigned.UInt.to_int info.quorum) ;
        Printf.printf "qdevice name %s\n" info.qdevice_name ;
        Ok ()
    | Error e ->
        failwith (to_string e)
  )
  |> ignore

let _test_quorumtool () =
  let open Quorumtool in
  update_membership_info AddressFormatName
  >>= (fun () ->
        print_int (ViewList.get_view_list_entries ()) ;
        print_newline () ;
        List.iter
          (fun n ->
            Printf.printf "nodeid %d node name %s"
              ViewList.(n.node_id)
              ViewList.(Option.get n.name) ;
            print_string " "
          )
          (ViewList.get_view_list ()) ;
        print_newline () ;
        Ok ()
      )
  |> ignore

let _test_cfg () =
  let open Cfg in
  ( with_handle @@ fun handle ->
    match cfg_get_node_addrs handle 1 with
    | Ok node_addrs ->
        List.iter
          (fun a -> Printf.printf "node addr %s and len %d\n" a.addr a.addr_len)
          node_addrs ;
        Ok ()
    | Error e ->
        failwith (to_string e)
  )
  |> ignore

let () = _test_quorumtool ()
