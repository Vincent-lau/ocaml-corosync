open Corosync_lib

type name_format = AddressFormatName | AddressFormatIP

module ViewList : sig
  type view_list_entry = {
      vq_info: Votequorum.vinfo option
    ; name: string option
    ; node_id: int
  }

  val get_view_list : unit -> view_list_entry list
  (** [get_view_list] does not return a result, i.e. it always succeeeds. The
      possibility of a [None], i.e. list not fully updated, is reflected by the
      [Error] when calling [update_membership_info] *)
end

val update_membership_info : name_format -> (unit, Corotypes.CsError.t) result
(** This function is run to update the membership info stored in the viewlist.
    Run it before retrieving view_list entries. *)

val is_quorate : unit -> (bool, Corotypes.CsError.t) result

val using_votequorum : unit -> bool

val votequorum_info : unit -> (Votequorum.vinfo, Corotypes.CsError.t) result
