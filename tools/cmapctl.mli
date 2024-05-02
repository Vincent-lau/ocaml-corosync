open Corosync_lib

val get : string -> (string, Corotypes.CsError.t) result
(** [get key] will retrieve the value associated with the [key] in corosync cmap *)

val get_prefix : string -> ((string * string) list, Corotypes.CsError.t) result
