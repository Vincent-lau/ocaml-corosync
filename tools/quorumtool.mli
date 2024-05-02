open Corosync_lib


type name_format = AddressFormatName | AddressFormatIP

module ViewList : sig
  type view_list_entry = {
      vq_info: Votequorum.vinfo option
    ; name: string option
    ; node_id: int
  }

  val get_view_list : unit -> view_list_entry list
end

val update_membership_info : name_format -> (unit, Corotypes.CsError.t) result

val is_quorate : unit -> (bool, Corotypes.CsError.t) result

val using_votequorum : unit -> bool

val votequorum_info : unit -> (Votequorum.vinfo, Corotypes.CsError.t) result
