open Ctypes
open Foreign
open Corotypes
open CsError

let ( >>= ) = Result.bind

let cmap_handle_t = uint64_t

let size_t = uint64_t

let cmap_value_types_t = int

module CmapValuetype = struct
  exception Unsupported_Valuetype of int

  type t =
    | CmapValInt8
    | CmapValUint8
    | CmapValInt16
    | CmapValUint16
    | CmapValInt32
    | CmapValUInt32
    | CmapValInt64
    | CmapValUint64
    | CmapValFloat
    | CmapValDouble
    | CmapValString
    | CmapValBinary

  let from_int = function
    | 1 ->
        CmapValInt8
    | 2 ->
        CmapValUint8
    | 3 ->
        CmapValInt16
    | 4 ->
        CmapValUint16
    | 5 ->
        CmapValInt32
    | 6 ->
        CmapValUInt32
    | 7 ->
        CmapValInt64
    | 8 ->
        CmapValUint64
    | 9 ->
        CmapValFloat
    | 10 ->
        CmapValDouble
    | 11 ->
        CmapValString
    | 12 ->
        CmapValBinary
    | n ->
        raise (Unsupported_Valuetype n)
end

module CmapRet = struct
  type t =
    | CmapInt8 of int
    | CmapUInt8 of Unsigned.uint8
    | CmapInt16 of int
    | CmapUInt16 of Unsigned.uint16
    | CmapInt32 of int32
    | CmapUInt32 of Unsigned.uint32
    | CmapInt64 of int64
    | CmapUInt64 of Unsigned.uint64
    | CmapFloat of float
    | CmapDouble of float (* ocaml does not have double def *)
    | CmapString of string
    | CmapBinary of Bytes.t

  let to_string = function
    | CmapInt8 i ->
        string_of_int i
    | CmapUInt8 i ->
        Unsigned.UInt8.to_string i
    | CmapInt16 i ->
        string_of_int i
    | CmapUInt16 i ->
        Unsigned.UInt16.to_string i
    | CmapInt32 i ->
        Int32.to_string i
    | CmapUInt32 i ->
        Unsigned.UInt32.to_string i
    | CmapInt64 i ->
        Int64.to_string i
    | CmapUInt64 i ->
        Unsigned.UInt64.to_string i
    | CmapFloat f | CmapDouble f ->
        Float.to_string f
    | CmapString s ->
        s
    | CmapBinary b ->
        Bytes.to_string b
end

let cmap_initialize =
  foreign "cmap_initialize" (ptr cmap_handle_t @-> returning cs_error_t)

let cmap_finalize =
  foreign "cmap_finalize" (cmap_handle_t @-> returning cs_error_t)

(* cmap functions *)

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

let cmap_get =
  foreign "cmap_get"
    (cmap_handle_t
    @-> string
    @-> ptr void
    @-> ptr size_t
    @-> ptr cmap_value_types_t
    @-> returning cs_error_t
    )

let cmap_iter_handle_t = uint64_t

let cmap_iter_init =
  foreign "cmap_iter_init"
    (cmap_handle_t
    @-> string
    @-> ptr cmap_iter_handle_t
    @-> returning cs_error_t
    )

let cmap_iter_next =
  foreign "cmap_iter_next"
    (cmap_handle_t
    @-> cmap_iter_handle_t
    @-> ptr char
    @-> ptr size_t
    @-> ptr cmap_value_types_t
    @-> returning cs_error_t
    )

let cmap_iter_finialize =
  foreign "cmap_iter_finalize"
    (cmap_handle_t @-> cmap_iter_handle_t @-> returning cs_error_t)

(* higher level get functions *)

open CmapRet

let get_int8 handle key =
  let res = allocate int8_t 0 in
  cmap_get_int8 handle key res |> to_result >>= fun () -> Ok (CmapInt8 !@res)

let get_uint8 handle key =
  let res = allocate uint8_t Unsigned.UInt8.zero in
  cmap_get_uint8 handle key res |> to_result >>= fun () -> Ok (CmapUInt8 !@res)

let get_int16 handle key =
  let res = allocate int16_t 0 in
  cmap_get_int16 handle key res |> to_result >>= fun () -> Ok (CmapInt16 !@res)

let get_uint16 handle key =
  let res = allocate uint16_t Unsigned.UInt16.zero in
  cmap_get_uint16 handle key res |> to_result >>= fun () -> Ok (CmapUInt16 !@res)

let get_int32 handle key =
  let res = allocate int32_t Int32.zero in
  cmap_get_int32 handle key res |> to_result >>= fun () -> Ok (CmapInt32 !@res)

let get_uint32 handle key =
  let res = allocate uint32_t Unsigned.UInt32.zero in
  cmap_get_uint32 handle key res |> to_result >>= fun () -> Ok (CmapUInt32 !@res)

let get_int64 handle key =
  let res = allocate int64_t Int64.zero in
  cmap_get_int64 handle key res |> to_result >>= fun () -> Ok (CmapInt64 !@res)

let get_uint64 handle key =
  let res = allocate uint64_t Unsigned.UInt64.zero in
  cmap_get_uint64 handle key res |> to_result >>= fun () -> Ok (CmapUInt64 !@res)

let get_string handle key =
  let res = allocate string "" in
  cmap_get_string handle key res |> to_result >>= fun () -> Ok (CmapString !@res)

let get_float handle key =
  let res = allocate float 0. in
  cmap_get_float handle key res |> to_result >>= fun () -> Ok (CmapFloat !@res)

let get_double handle key =
  let res = allocate float 0. in
  cmap_get_double handle key res |> to_result >>= fun () -> Ok (CmapDouble !@res)

let get_by_type = function
  | CmapValuetype.CmapValInt8 ->
      get_int8
  | CmapValuetype.CmapValUint8 ->
      get_uint8
  | CmapValuetype.CmapValInt16 ->
      get_int16
  | CmapValuetype.CmapValUint16 ->
      get_uint16
  | CmapValuetype.CmapValInt32 ->
      get_int32
  | CmapValuetype.CmapValUInt32 ->
      get_uint32
  | CmapValuetype.CmapValInt64 ->
      get_int64
  | CmapValuetype.CmapValUint64 ->
      get_uint64
  | CmapValuetype.CmapValFloat ->
      get_float
  | CmapValuetype.CmapValDouble ->
      get_double
  | CmapValuetype.CmapValString ->
      get_string
  | CmapValuetype.CmapValBinary ->
      failwith "CmapValBinary Unimplemented"

let get handle key =
  let value_len = allocate size_t Unsigned.UInt64.zero in
  let value_type = allocate cmap_value_types_t 0 in
  cmap_get handle key null value_len value_type |> CsError.to_result
  >>= fun () ->
  let val_typ = CmapValuetype.from_int !@value_type in
  get_by_type val_typ handle key

let rec get_prefix_rec handle prefix iter_handle =
  let value_len = allocate size_t Unsigned.UInt64.zero in
  let value_type = allocate cmap_value_types_t 0 in
  let key_arr = CArray.make char 256 in
  let key = CArray.start key_arr in
  match
    cmap_iter_next handle iter_handle key value_len value_type
    |> CsError.from_int
  with
  | CsOk ->
      let key_name = Ctypes_std_views.string_of_char_ptr key in
      let val_typ = CmapValuetype.from_int !@value_type in
      get_by_type val_typ handle key_name >>= fun hd ->
      get_prefix_rec handle prefix iter_handle >>= fun tl ->
      Ok ((key_name, hd) :: tl)
  | CsErrNoSections ->
      (* no more sections to iterate *)
      Ok []
  | e ->
      Error e

let get_prefix handle prefix =
  let iter_handle = allocate cmap_iter_handle_t Unsigned.UInt64.zero in
  cmap_iter_init handle prefix iter_handle |> CsError.to_result >>= fun () ->
  get_prefix_rec handle prefix !@iter_handle >>= fun r ->
  cmap_iter_finialize handle !@iter_handle |> CsError.to_result >>= fun () ->
  Ok r

(** with_handle will take a function f which takes a handle and performs operations
  with that handle. with_handle f will automatically create that handle and pass
  it to f and close that handle afterwards *)
let with_handle f =
  let handle = allocate cmap_handle_t Unsigned.UInt64.zero in
  cmap_initialize handle |> CsError.to_result >>= fun () ->
  let r = f !@handle in
  cmap_finalize !@handle |> CsError.to_result >>= fun () -> r
