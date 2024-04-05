open Corosync

let _test_cmap () =
  let open Cmap in
  ( with_handle @@ fun handle ->
    Cmap.get_prefix handle "totem" |> function
    | Ok l ->
        List.iter
          (fun (k, v) -> Printf.printf "%s: %s\n" k (CmapRet.to_string v))
          l ;
        Ok ()
    | Error e ->
        failwith (Cserror.to_string e)
  )
  |> ignore

let _test_quorum () =
  let open Quorum in
  ( with_handle @@ fun handle ->
    match getquorate handle with
    | Ok b ->
        Printf.printf "is quorate %b\n" b ;
        Ok ()
    | Error e ->
        failwith (Cserror.to_string e)
  )
  |> ignore

let () =
  let open Votequorum in
  ( with_handle @@ fun handle ->
    match getinfo handle with
    | Ok info ->
        Printf.printf "total votes %d\n" (Unsigned.UInt.to_int info.total_votes) ;
        Printf.printf "quorum %d\n" (Unsigned.UInt.to_int info.quorum) ;
        Printf.printf "qdevice name %s\n" info.qdevice_name;
        Ok ()
    | Error e ->
        failwith (Cserror.to_string e)
  )
  |> ignore
