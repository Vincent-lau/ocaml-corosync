open Ctypes
open Foreign
open Corotypes
open CsError

let ( >>= ) = Result.bind

let cpg_handle_t = uint64_t

type cpg_name

let cpg_name : cpg_name structure typ = structure "cpg_name"

let cpg_name_length = field cpg_name "length" uint32_t

let cpg_name_value = field cpg_name "value" (ptr char)

let () = seal cpg_name

type cpg_address

let cpg_address : cpg_address structure typ = structure "cpg_address"

let nodeid = field cpg_address "nodeid" uint32_t

let pid = field cpg_address "pid" uint32_t

let reason = field cpg_address "reason" uint32_t

let () = seal cpg_address

let cpg_deliver_fn_t =
  cpg_handle_t @-> ptr cpg_name @-> uint32_t @-> uint32_t @-> returning void

let cpg_confchg_fn_t =
  cpg_handle_t
  @-> ptr cpg_name
  @-> ptr cpg_address
  @-> size_t
  @-> ptr cpg_address
  @-> size_t
  @-> cpg_address
  @-> size_t
  @-> returning void

(* struct cpg_callbacks_t *)

type cpg_callbacks_t

let cpg_callbacks_t : cpg_callbacks_t structure typ =
  structure "cpg_callbacks_t"

let cpg_deliver_fn =
  field cpg_callbacks_t "cpg_deliver_fn" (funptr cpg_deliver_fn_t)

let cpg_confchg_fn =
  field cpg_callbacks_t "cpg_confchg_fn" (funptr cpg_confchg_fn_t)

let () = seal cpg_callbacks_t

(* cpg bindings *)

let cpg_initialize =
  foreign "cpg_initialize"
    (ptr cpg_handle_t @-> ptr cpg_callbacks_t @-> returning cs_error_t)

let cpg_finalize = foreign "cpg_finalize" (cpg_handle_t @-> returning cs_error_t)

let cpg_fd_get =
  foreign "cpg_fd_get" (cpg_handle_t @-> ptr int @-> returning cs_error_t)

let cpg_join =
  foreign "cpg_join" (cpg_handle_t @-> ptr cpg_name @-> returning cs_error_t)

let cpg_leave =
  foreign "cpg_leave" (cpg_handle_t @-> ptr cpg_name @-> returning cs_error_t)

let cpg_membership_get =
  foreign "cpg_membership_get"
    (cpg_handle_t
    @-> ptr cpg_name
    @-> ptr cpg_address
    @-> ptr int
    @-> returning cs_error_t
    )

let cpg_local_get =
  foreign "cpg_local_get" (cpg_handle_t @-> ptr uint @-> returning cs_error_t)

(* ocaml APIs *)

let with_handle f =
  let handle = allocate cpg_handle_t Unsigned.UInt64.zero in
  cpg_initialize handle (from_voidp cpg_callbacks_t null) |> CsError.to_result
  >>= fun () ->
  let r = f !@handle in
  cpg_finalize !@handle |> CsError.to_result >>= fun () -> r

let fd_get handle =
  let fd = allocate int 0 in
  cpg_fd_get handle fd |> to_result >>= fun () -> Ok !@fd

let local_get handle =
  let local_nodeid = allocate uint Unsigned.UInt.zero in
  cpg_local_get handle local_nodeid |> to_result >>= fun () -> Ok !@local_nodeid
