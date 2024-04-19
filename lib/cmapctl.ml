open Cmap

let get key = with_handle @@ fun handle -> Cmap.get handle key

let get_prefix prefix =
  with_handle @@ fun handle -> Cmap.get_prefix handle prefix
