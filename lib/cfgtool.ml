open Cfg

let local_get () = with_handle @@ cfg_local_get

let reload_config () = with_handle @@ cfg_reload_config

let get_node_addrs nodeid =
  with_handle @@ fun handle -> cfg_get_node_addrs handle nodeid
