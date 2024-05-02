open Corosync_lib
open Cmap

let get key = with_handle @@ fun handle -> get handle key

let get_prefix prefix =
  with_handle @@ fun handle -> get_prefix handle prefix
