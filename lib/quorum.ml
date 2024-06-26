open Ctypes
open Foreign
open Corotypes
open CsError

let ( >>= ) = Result.bind

let quorum_handle_t = uint64_t

let quorum_notification_fn_t =
  quorum_handle_t
  @-> uint32_t
  @-> uint64_t
  @-> uint32_t
  @-> ptr uint32_t
  @-> returning void

type quorum_callbacks_t

let quorum_callbacks_t : quorum_callbacks_t structure typ =
  structure "quorum_callbacks_t"

let quorum_notify_fn =
  field quorum_callbacks_t "quorum_notify_fn" (funptr quorum_notification_fn_t)

let () = seal quorum_callbacks_t

(* binding to libquorum *)

let quorum_initialize =
  foreign "quorum_initialize"
    (ptr quorum_handle_t
    @-> ptr quorum_callbacks_t
    @-> ptr uint32_t
    @-> returning cs_error_t
    )

let quorum_finalize =
  foreign "quorum_finalize" (quorum_handle_t @-> returning cs_error_t)

let quorum_dispatch =
  foreign "quorum_dispatch"
    (quorum_handle_t
    @-> CsDispatchFlag.cs_dispatch_flags_t
    @-> returning cs_error_t
    )

let quorum_trackstart =
  foreign "quorum_trackstart" (quorum_handle_t @-> uint @-> returning cs_error_t)

let quorum_trackstop =
  foreign "quorum_trackstop" (quorum_handle_t @-> returning cs_error_t)

let quorum_getquorate =
  foreign "quorum_getquorate"
    (quorum_handle_t @-> ptr int @-> returning cs_error_t)

(* exposed functions *)

let getquorate handle =
  let is_quorate = allocate int 0 in
  quorum_getquorate handle is_quorate |> to_result >>= fun () ->
  match !@is_quorate with
  | 1 ->
      Ok true
  | 0 ->
      Ok false
  | _ ->
      Error CsErrOcamlCompat

let with_handle f =
  let qhandle = allocate quorum_handle_t Unsigned.UInt64.zero in
  let qtype = allocate uint32_t Unsigned.UInt32.zero in
  quorum_initialize qhandle (from_voidp quorum_callbacks_t null) qtype
  |> CsError.to_result
  >>= fun () ->
  let r = f !@qhandle in
  quorum_finalize !@qhandle |> CsError.to_result >>= fun () -> r
