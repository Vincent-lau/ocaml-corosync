open Corosync_lib
open Cmap

val get : 'a CmapValue.value -> string -> ('a, Corotypes.CsError.t) result
(** [get key] will retrieve the value associated with the [key] in corosync cmap *)

val set : string -> 'a CmapValue.value -> (unit, Corotypes.CsError.t) result

val get_prefix : string -> ((string * string) list, Corotypes.CsError.t) result
