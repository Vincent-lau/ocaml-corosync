open Ctypes
open Foreign
open Corotypes
open CsError

let ( >>= ) = Result.bind

let votequorum_qdevice_max_name_len = 255

let votequorum_handle_t = uint64_t

(* struct votequorum_node_t *)
type votequorum_node_t

let votequorm_node_t : votequorum_node_t structure typ =
  structure "votequorum_node_t"

let vnode_nodeid = field votequorm_node_t "nodeid" uint32_t

let vnode_state = field votequorm_node_t "state" uint32_t

let () = seal votequorm_node_t

(* struct vote_ring_id_t *)
type votequorum_ring_id_t

let votequorum_ring_id_t : votequorum_ring_id_t structure typ =
  structure "votequorum_ring_id_t"

let ring_nodeid = field votequorum_ring_id_t "nodeid" uint32_t

let ring_seq = field votequorum_ring_id_t "seq" uint64_t

let () = seal votequorum_ring_id_t

let votequorum_quorum_notification_fn_t =
  votequorum_handle_t
  @-> uint64_t
  @-> uint32_t
  @-> uint32_t
  @-> ptr votequorm_node_t
  @-> returning (ptr void)

let votequorum_nodelist_notification_fn_t =
  votequorum_handle_t
  @-> uint64_t
  @-> votequorum_ring_id_t
  @-> uint32_t
  @-> returning (ptr void)

let votequorum_expectedvotes_notification_fn_t =
  votequorum_handle_t @-> uint64_t @-> uint32_t @-> returning (ptr void)

(* votequorum_callbacks_t *)
type votequorum_callbacks_t

let votequorum_callbacks_t : votequorum_callbacks_t structure typ =
  structure "votequorum_callbacks_t"

let votequorum_quorum_notify_fn =
  field votequorum_callbacks_t "votequorum_quorum_notify_fn"
    (funptr votequorum_quorum_notification_fn_t)

let votequorum_expectedvotes_notify_fn =
  field votequorum_callbacks_t "votequorum_expectedvotes_notify_fn"
    (funptr votequorum_nodelist_notification_fn_t)

let votequorum_nodelist_notify_fn =
  field votequorum_callbacks_t "votequorum_nodelist_notify_fn"
    (funptr votequorum_nodelist_notification_fn_t)

let () = seal votequorum_callbacks_t

(* struct votequorum_info *)
type votequorum_info

let votequorum_info : votequorum_info structure typ =
  structure "votequorum_info"

let node_id = field votequorum_info "node_id" uint

let node_state = field votequorum_info "node_state" uint

let node_votes = field votequorum_info "node_votes" uint

let node_expected_votes = field votequorum_info "node_expected_votes" uint

let highest_votes = field votequorum_info "highest_votes" uint

let total_votes = field votequorum_info "total_votes" uint

let quorum = field votequorum_info "quorum" uint

let flags = field votequorum_info "flags" uint

let qdevice_votes = field votequorum_info "qdevice_votes" uint

let qdevice_name =
  field votequorum_info "qdevice_name"
    (array votequorum_qdevice_max_name_len char)

let () = seal votequorum_info

(* initialization functions *)
let votequorum_initialize =
  foreign "votequorum_initialize"
    (ptr votequorum_handle_t
    @-> ptr votequorum_callbacks_t
    @-> returning cs_error_t
    )

let votequorum_finalize =
  foreign "votequorum_finalize" (votequorum_handle_t @-> returning cs_error_t)

let votequorum_getinfo =
  foreign "votequorum_getinfo"
    (votequorum_handle_t
    @-> uint
    @-> ptr votequorum_info
    @-> returning cs_error_t
    )

type vinfo = {
    node_id: Unsigned.uint
  ; node_state: Unsigned.uint
  ; node_votes: Unsigned.uint
  ; node_expected_votes: Unsigned.uint
  ; highest_votes: Unsigned.uint
  ; total_votes: Unsigned.uint
  ; quorum: Unsigned.uint
  ; flags: Unsigned.uint
  ; qdevice_votes: Unsigned.uint
  ; qdevice_name: string
}

let get_info handle nodeid =
  let info = make votequorum_info in
  votequorum_getinfo handle (Unsigned.UInt.of_int nodeid) (addr info)
  |> to_result
  >>= fun () ->
  let qdevice_namef = getf info qdevice_name in
  let qdevice_name =
    CArray.start qdevice_namef |> Ctypes_std_views.string_of_char_ptr
  in
  (* make sure that qdevice_namef does not get collected until string_of_char_ptr is done *)
  Ctypes_memory_stubs.use_value qdevice_namef ;
  Ok
    {
      node_id= getf info node_id
    ; node_state= getf info node_state
    ; node_votes= getf info node_votes
    ; node_expected_votes= getf info node_expected_votes
    ; highest_votes= getf info highest_votes
    ; total_votes= getf info total_votes
    ; quorum= getf info quorum
    ; flags= getf info flags
    ; qdevice_votes= getf info qdevice_votes
    ; qdevice_name
    }

let get_my_info handle =
  Cfg.(with_handle cfg_local_get) >>= fun me -> get_info handle me

let with_handle f =
  let handle = allocate votequorum_handle_t Unsigned.UInt64.zero in
  votequorum_initialize handle (from_voidp votequorum_callbacks_t null)
  |> to_result
  >>= fun () ->
  let r = f !@handle in
  votequorum_finalize !@handle |> to_result >>= fun () -> r
