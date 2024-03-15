open Corosync

let () =
  let open Cmap in
  ( with_handle @@ fun handle ->
    Cmap.get_prefix handle "totem" |> function
    | Ok l ->
        List.iter
          (fun (k, v) -> Printf.printf "%s: %s\n" k (CmapRet.to_string v))
          l ;
        Ok ()
    | Error e ->
        failwith (CmapError.to_string e)
  )
  |> ignore
