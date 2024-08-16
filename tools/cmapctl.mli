open Corosync_lib
open Cmap

val get : 'a CmapValue.value -> string -> ('a, Corotypes.CsError.t) result
(** [get key] will retrieve the value associated with the [key] in corosync cmap.
User of this function should understand what the underlying type of each key is
and supply that type into this function. The type information for cmap can be
retrieved by doing a corosync-cmapctl call and examining the types. *)

val set : string -> 'a CmapValue.value -> (unit, Corotypes.CsError.t) result
(** [set key value] sets the cmap in memory database [key] to [value] *)

val get_prefix : string -> ((string * string) list, Corotypes.CsError.t) result
(** [get_prefix prefix] will return a list of (k,v) where k has a prefix of [prefix].
It is recommended to use the [get] function where possible since that function
is more type safe. *)
