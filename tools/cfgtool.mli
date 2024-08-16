open Corosync_lib

val local_get : unit -> (int, Corotypes.CsError.t) result
(** [local_get ()] returns the nodeid of the node on which this function is called. *)

val reload_config : unit -> (unit, Corotypes.CsError.t) result
(** [realod_config ()] will ask corosync to reload config on the current node. *)

val get_node_addrs :
  int -> (Cfg.cfg_node_address list, Corotypes.CsError.t) result
(** [get_node_addrs nodeid] returns the addresses of the node with id [nodeid]. 
Note that Corosync 3 supports multiple homing, so there could be multiple addresses for one node. *)
