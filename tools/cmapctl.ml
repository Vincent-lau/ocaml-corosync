open Corosync_lib
open Cmap

let get value key = with_handle @@ fun handle -> get value handle key

let set key value = with_handle @@ fun handle -> set handle key value

let get_prefix prefix = with_handle @@ fun handle -> get_prefix handle prefix
