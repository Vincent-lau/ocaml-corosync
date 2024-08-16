open Corosync_lib
open Corotypes

type name_format = AddressFormatName | AddressFormatIP

module ViewList : sig
  type node_name = Name of string | Ips of Ipaddr.t list

  type view_list_entry = {
      vq_info: Votequorum.vinfo option
    ; name: node_name option
    ; node_id: int
  }

  val get_view_list : unit -> view_list_entry list
  (** [get_view_list] does not return a result, i.e. it always succeeeds. The
      possibility of a [None], i.e. list not fully updated, is reflected by the
      [Error] when calling [update_membership_info] *)
end

val is_quorate : unit -> (bool, CsError.t) result
(** [is_quorate () returns whether the current cluster is quorate ]*)

val using_votequorum : unit -> bool
(** [using_votequorum ()] returns whether votequorum is used in the current cluster *)

val votequorum_info : int -> (Votequorum.vinfo, CsError.t) result
(** [votequorum_info nodeid] returns the votequoruminfo for the node with [nodeid] *)

val my_votequorum_info : unit -> (Votequorum.vinfo, CsError.t) result
(** [my_votequorum_info ()] returns the votequoruminfo for the node on which this
function is called *)

val quorum_members :
  name_format -> (ViewList.view_list_entry list, CsError.t) result
(** [quorum_members format] returns the members and their information in the current 
quorum as a list. The format can either be an ip address or the host name. *)
