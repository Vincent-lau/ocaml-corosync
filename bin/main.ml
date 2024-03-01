open Corosync

let () =
  match
    Cmap.with_handle (fun handle -> Cmap.get handle "totem.cluster_name")
  with
  | Ok (CmapString s) ->
      print_endline s
  | Error e ->
      Cmap.reterr_to_string e |> failwith
  | _ ->
      failwith "Unimplmeneted"
