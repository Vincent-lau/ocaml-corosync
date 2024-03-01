open Ctypes
open Foreign

let ( >>= ) = Result.bind

let cmap_handle_t = uint64_t

let size_t = uint64_t

(* TODO change to enum types *)
let cmap_value_types_t = int

let cs_error_t = int

type cmap_ret_error =
  | CsOk
  | CsErrLibrary
  | CsErrVersion
  | CsErrInit
  | CsErrTimeout
  | CsErrTryAgain
  | CsErrInvalidParam
  | CsErrNoMemory
  | CsErrBadHandle
  | CsErrBusy
  | CsErrAccess (* 11 *)
  | CsErrNotExist (* 12 *)
  | CsErrNameTooLong (* 13 *)
  | CsErrExist (* 14 *)
  | CsErrNoSpace (* 15 *)
  | CsErrInterupt
  | CsErrNameNotFound
  | CsErrNoResources
  | CsErrNotSupported
  | CsErrBadOperation
  | CsErrFailedOperation
  | CsErrMessageError
  | CsErrQueueFull
  | CsErrQueueNotAvailable
  | CsErrBadFlags
  | CsErrTooBig (* 26 *)
  | CsErrNoSections (* 27 *)
  | CsErrContextNotFound (* 28 *)
  | CsErrTooManyGroups (* 30 *)
  | CsErrSecurity (* 100 *)

let parse_ret_err = function
  | 1 ->
      Ok ()
  | 2 ->
      Error CsErrLibrary
  | 3 ->
      Error CsErrVersion
  (* TODO finish this *)
  | e ->
      Printf.sprintf "unknown error %d" e |> failwith

let reterr_to_string = function
  | CsOk -> "cs_ok"
  | CsErrLibrary -> "cs_err"
  | _ -> failwith "Unimplemented"

type cmap_ret_type =
  | CmapInt8 of int
  | CmapUint8 of Unsigned.uint8
  | CmapInt16 of int
  | CmapUint16 of Unsigned.uint16
  | CmapInt32 of Unsigned.uint32
  | CmapUint32 of int32
  | CmapInt64 of int64
  | CmapUint64 of Unsigned.uint64
  | CmapFloat of float
  | CmapDouble of float (* ocaml does not have double def *)
  | CmapString of string
  | CmapBinary of Bytes.t

let cmap_initialize =
  foreign "cmap_initialize" (ptr cmap_handle_t @-> returning cs_error_t)

let cmap_get_int8 =
  foreign "cmap_get_int8"
    (cmap_handle_t @-> string @-> ptr int8_t @-> returning cs_error_t)

let cmap_get_uint8 =
  foreign "cmap_get_uint8"
    (cmap_handle_t @-> string @-> ptr uint8_t @-> returning cs_error_t)

let cmap_get_int16 =
  foreign "cmap_get_int16"
    (cmap_handle_t @-> string @-> ptr int16_t @-> returning cs_error_t)

let cmap_get_uint16 =
  foreign "cmap_get_uint16"
    (cmap_handle_t @-> string @-> ptr uint16_t @-> returning cs_error_t)

let cmap_get_int32 =
  foreign "cmap_get_int32"
    (cmap_handle_t @-> string @-> ptr int32_t @-> returning cs_error_t)

let cmap_get_uint32 =
  foreign "cmap_get_uint32"
    (cmap_handle_t @-> string @-> ptr uint32_t @-> returning cs_error_t)

let cmap_get_int64 =
  foreign "cmap_get_int64"
    (cmap_handle_t @-> string @-> ptr int64_t @-> returning cs_error_t)

let cmap_get_uint64 =
  foreign "cmap_get_uint64"
    (cmap_handle_t @-> string @-> ptr uint64_t @-> returning cs_error_t)

let cmap_get_float =
  foreign "cmap_get_float"
    (cmap_handle_t @-> string @-> ptr float @-> returning cs_error_t)

let cmap_get_double =
  foreign "cmap_get_double"
    (cmap_handle_t @-> string @-> ptr double @-> returning cs_error_t)

let cmap_get_string =
  foreign "cmap_get_string"
    (cmap_handle_t @-> string @-> ptr string @-> returning cs_error_t)

let cmap_finalize =
  foreign "cmap_finalize" (cmap_handle_t @-> returning cs_error_t)

let cmap_get =
  foreign "cmap_get"
    (cmap_handle_t
    @-> string
    @-> ptr void
    @-> ptr size_t
    @-> ptr cmap_value_types_t
    @-> returning cs_error_t
    )

let get_int8 handle key =
  let res = allocate int8_t 0 in
  cmap_get_int8 !@handle key res |> parse_ret_err >>= fun () ->
  Ok (CmapInt8 !@res)

let get_uint8 handle key =
  let res = allocate uint8_t Unsigned.UInt8.zero in
  cmap_get_uint8 !@handle key res |> parse_ret_err >>= fun () ->
  Ok (CmapUint8 !@res)

let get_string handle key =
  let res = allocate string "" in
  cmap_get_string !@handle key res |> parse_ret_err >>= fun () ->
  Ok (CmapString !@res)

let get_by_type handle key = function
  | 1 ->
      get_int8 handle key
  | 2 ->
      get_uint8 handle key
  (* TODO finish this *)
  | 11 ->
      get_string handle key
  | _ ->
      failwith "Unknown type"

let get handle key : (cmap_ret_type, cmap_ret_error) Result.t =
  let value_len = allocate size_t Unsigned.UInt64.zero in
  let value_type = allocate cmap_value_types_t 0 in
  let _e = cmap_get !@handle key null value_len value_type in
  get_by_type handle key !@value_type

let with_handle get_fun =
  let handle = allocate cmap_handle_t Unsigned.UInt64.zero in
  let _e = cmap_initialize handle in

  let r = get_fun handle in
  ignore @@ cmap_finalize !@handle ;
  r

(* let res = allocate string "" in

   let _e = cmap_get_string !@handle key res in
   print_endline !@res ; *)
