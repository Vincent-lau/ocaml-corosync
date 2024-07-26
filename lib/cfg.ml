open Ctypes
open Foreign
open Corotypes
open CsError

let ( >>= ) = Result.bind

(* preliminary type definitions *)

let corosync_cfg_handle_t = uint64_t

type corosync_cfg_callbacks_t

type corosync_cfg_node_address_t

let corosync_cfg_node_address_t : corosync_cfg_node_address_t structure typ =
  structure "corosync_cfg_node_address_t"

let address_length = field corosync_cfg_node_address_t "address_length" int

(* TODO encode sizeof(struct sockaddr_in6) *)
let address = field corosync_cfg_node_address_t "address" (array 30 char)

let () = seal corosync_cfg_node_address_t

(* TODO integrate this *)
module Corosync_cfg_shutdown_flags = struct
  exception Unknown_Shutdown_Flag of int

  type t =
    | CorosyncCfgShutdownFlagRequst
    | CorosyncCfgShutdownFlagRegardless
    | CorosyncCfgShutdownFlagImmediate

  let from_int = function
    | 0 ->
        CorosyncCfgShutdownFlagRequst
    | 1 ->
        CorosyncCfgShutdownFlagRegardless
    | 2 ->
        CorosyncCfgShutdownFlagImmediate
    | n ->
        raise (Unknown_Shutdown_Flag n)
end

let corosync_cfg_shutdown_flags_t = int

let corosync_cfg_shutdown_callback_t =
  corosync_cfg_handle_t @-> corosync_cfg_shutdown_flags_t @-> returning void

let corosync_cfg_callbacks_t : corosync_cfg_callbacks_t structure typ =
  structure "corosync_cfg_callbacks_t"

let corosync_cfg_shutdown_callback =
  field corosync_cfg_callbacks_t "corosync_cfg_shutdown_callback"
    (funptr corosync_cfg_shutdown_callback_t)

let () = seal corosync_cfg_callbacks_t

(* bindings to cfg.h *)

let corosync_cfg_initialize =
  foreign "corosync_cfg_initialize"
    (ptr corosync_cfg_handle_t
    @-> ptr corosync_cfg_callbacks_t
    @-> returning cs_error_t
    )

let corosync_cfg_finalize =
  foreign "corosync_cfg_finalize"
    (corosync_cfg_handle_t @-> returning cs_error_t)

let corosync_cfg_local_get =
  foreign "corosync_cfg_local_get"
    (corosync_cfg_handle_t @-> ptr uint @-> returning cs_error_t)

let corosync_cfg_get_node_addrs =
  foreign "corosync_cfg_get_node_addrs"
    (corosync_cfg_handle_t
    @-> uint
    @-> uint64_t
    @-> ptr int
    @-> ptr corosync_cfg_node_address_t
    @-> returning cs_error_t
    )

let corosync_cfg_reload_config =
  foreign "corosync_cfg_reload_config"
    (corosync_cfg_handle_t @-> returning cs_error_t)

(* wrapper and exposed functions *)

let cfg_local_get handle =
  let local_nodeid = allocate uint Unsigned.UInt.zero in
  corosync_cfg_local_get handle local_nodeid |> to_result >>= fun () ->
  Ok (Unsigned.UInt.to_int !@local_nodeid)

type cfg_node_address = {addr_len: int; addr: string}

let cfg_get_node_addrs chandle nodeid =
  let num_addrs = allocate int 0 in
  let addrs = CArray.make corosync_cfg_node_address_t interface_max in
  corosync_cfg_get_node_addrs chandle
    (Unsigned.UInt.of_int nodeid)
    (Unsigned.UInt64.of_int interface_max)
    num_addrs (CArray.start addrs)
  |> to_result
  >>= fun () ->
  Ok
    (CArray.fold_left
       (fun acc a ->
         let addr_len = getf a address_length in
         let addr =
           getf a address |> CArray.start |> Ctypes_std_views.string_of_char_ptr
         in
         Ctypes_memory_stubs.use_value a;
         {addr_len; addr} :: acc
       )
       [] addrs
    )

let cfg_reload_config handle = corosync_cfg_reload_config handle |> to_result

let with_handle f =
  let handle = allocate corosync_cfg_handle_t Unsigned.UInt64.zero in
  corosync_cfg_initialize handle (from_voidp corosync_cfg_callbacks_t null)
  |> to_result
  >>= fun () ->
  let r = f !@handle in
  corosync_cfg_finalize !@handle |> to_result >>= fun () -> r
