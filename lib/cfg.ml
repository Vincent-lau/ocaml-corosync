open Ctypes
open Foreign
open Cserror

let ( >>= ) = Result.bind

(* preliminary type definitions *)

let corosync_cfg_handle_t = uint64_t

type corosync_cfg_callbacks_t

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

(* TODO implement cfg reload *)

(* wrapper and exposed functions *)

let cfg_local_get handle =
  let local_nodeid = allocate uint Unsigned.UInt.zero in
  corosync_cfg_local_get handle local_nodeid |> Cserror.to_result >>= fun () ->
  Ok !@local_nodeid

let with_handle f =
  let handle = allocate corosync_cfg_handle_t Unsigned.UInt64.zero in
  corosync_cfg_initialize handle (from_voidp corosync_cfg_callbacks_t null)
  |> Cserror.to_result
  >>= fun () ->
  let r = f !@handle in
  corosync_cfg_finalize !@handle |> Cserror.to_result >>= fun () -> r
