open Corosync_lib

val local_get : unit -> (int, Corotypes.CsError.t) result

val reload_config : unit -> (unit, Corotypes.CsError.t) result

val get_node_addrs :
  int -> (Cfg.cfg_node_address list, Corotypes.CsError.t) result
