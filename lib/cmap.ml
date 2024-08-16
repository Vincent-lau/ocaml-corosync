open Ctypes
open Foreign
open Corotypes
open CsError

let ( >>= ) = Result.bind

let cmap_handle_t = uint64_t

type handle = Unsigned.UInt64.t

let size_t = uint64_t

let cmap_value_types_t = int

(* ------------------------------- cmap functions -------------------------------*)

let cmap_initialize =
  foreign "cmap_initialize" (ptr cmap_handle_t @-> returning cs_error_t)

let cmap_finalize =
  foreign "cmap_finalize" (cmap_handle_t @-> returning cs_error_t)

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

let cmap_set_int8 =
  foreign "cmap_set_int8"
    (cmap_handle_t @-> string @-> int8_t @-> returning cs_error_t)

let cmap_set_uint8 =
  foreign "cmap_set_uint8"
    (cmap_handle_t @-> string @-> uint8_t @-> returning cs_error_t)

let cmap_set_int16 =
  foreign "cmap_set_int16"
    (cmap_handle_t @-> string @-> int16_t @-> returning cs_error_t)

let cmap_set_uint16 =
  foreign "cmap_set_uint16"
    (cmap_handle_t @-> string @-> uint16_t @-> returning cs_error_t)

let cmap_set_int32 =
  foreign "cmap_set_int32"
    (cmap_handle_t @-> string @-> int32_t @-> returning cs_error_t)

let cmap_set_uint32 =
  foreign "cmap_set_uint32"
    (cmap_handle_t @-> string @-> uint32_t @-> returning cs_error_t)

let cmap_set_int64 =
  foreign "cmap_set_int64"
    (cmap_handle_t @-> string @-> int64_t @-> returning cs_error_t)

let cmap_set_uint64 =
  foreign "cmap_set_uint64"
    (cmap_handle_t @-> string @-> uint64_t @-> returning cs_error_t)

let cmap_set_float =
  foreign "cmap_set_float"
    (cmap_handle_t @-> string @-> float @-> returning cs_error_t)

let cmap_set_double =
  foreign "cmap_set_double"
    (cmap_handle_t @-> string @-> double @-> returning cs_error_t)

let cmap_set_string =
  foreign "cmap_set_string"
    (cmap_handle_t @-> string @-> string @-> returning cs_error_t)

let cmap_set =
  foreign "cmap_set"
    (cmap_handle_t
    @-> string
    @-> ptr void
    @-> size_t
    @-> cmap_value_types_t
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

(* ---------------------- higher level functions --------------------------*)

module CmapValue = struct
  exception Unsupported_Valuetype of int

  type _ value =
    | CmapInt8 : int -> int value
    | CmapUInt8 : Unsigned.uint8 -> Unsigned.uint8 value
    | CmapInt16 : int -> int value
    | CmapUInt16 : Unsigned.uint16 -> Unsigned.uint16 value
    | CmapInt32 : int32 -> int32 value
    | CmapUInt32 : Unsigned.uint32 -> Unsigned.uint32 value
    | CmapInt64 : int64 -> int64 value
    | CmapUInt64 : Unsigned.uint64 -> Unsigned.uint64 value
    | CmapFloat : float -> float value
    | CmapDouble : float -> float value
    | CmapString : string -> string value
    | CmapBinary : Bytes.t -> Bytes.t value

  (** existential type as an intermediate for converting a value to string *)
  type ex = E : 'a value -> ex

  (* convenience values for others to call the get function *)
  let int8 = CmapInt8 0

  let uint8 = CmapUInt8 Unsigned.UInt8.zero

  let int16 = CmapInt16 0

  let uint16 = CmapUInt16 Unsigned.UInt16.zero

  let int32 = CmapInt32 0l

  let uint32 = CmapUInt32 Unsigned.UInt32.zero

  let int64 = CmapInt64 0L

  let uint64 = CmapUInt64 Unsigned.UInt64.zero

  let float = CmapFloat 0.0

  let double = CmapDouble 0.0

  let string = CmapString ""

  let bytes = CmapBinary Bytes.empty

  let to_int : type a. a value -> int = function
    | CmapInt8 _ ->
        1
    | CmapUInt8 _ ->
        2
    | CmapInt16 _ ->
        3
    | CmapUInt16 _ ->
        4
    | CmapInt32 _ ->
        5
    | CmapUInt32 _ ->
        6
    | CmapInt64 _ ->
        7
    | CmapUInt64 _ ->
        8
    | CmapFloat _ ->
        9
    | CmapDouble _ ->
        10
    | CmapString _ ->
        11
    | CmapBinary _ ->
        12

  let to_string : type a. a value -> string = function
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

let get_int8 handle key =
  let res = allocate int8_t 0 in
  cmap_get_int8 handle key res |> to_result >>= fun () -> Ok !@res

let get_uint8 handle key =
  let res = allocate uint8_t Unsigned.UInt8.zero in
  cmap_get_uint8 handle key res |> to_result >>= fun () -> Ok !@res

let get_int16 handle key =
  let res = allocate int16_t 0 in
  cmap_get_int16 handle key res |> to_result >>= fun () -> Ok !@res

let get_uint16 handle key =
  let res = allocate uint16_t Unsigned.UInt16.zero in
  cmap_get_uint16 handle key res |> to_result >>= fun () -> Ok !@res

let get_int32 handle key =
  let res = allocate int32_t Int32.zero in
  cmap_get_int32 handle key res |> to_result >>= fun () -> Ok !@res

let get_uint32 handle key =
  let res = allocate uint32_t Unsigned.UInt32.zero in
  cmap_get_uint32 handle key res |> to_result >>= fun () -> Ok !@res

let get_int64 handle key =
  let res = allocate int64_t Int64.zero in
  cmap_get_int64 handle key res |> to_result >>= fun () -> Ok !@res

let get_uint64 handle key =
  let res = allocate uint64_t Unsigned.UInt64.zero in
  cmap_get_uint64 handle key res |> to_result >>= fun () -> Ok !@res

let get_string handle key =
  let res = allocate string "" in
  cmap_get_string handle key res |> to_result >>= fun () -> Ok !@res

let get_float handle key =
  let res = allocate float 0. in
  cmap_get_float handle key res |> to_result >>= fun () -> Ok !@res

let get_double handle key =
  let res = allocate float 0. in
  cmap_get_double handle key res |> to_result >>= fun () -> Ok !@res

open CmapValue

let get_val : type a. a value -> handle -> string -> (a, CsError.t) Result.t =
 fun value handle key ->
  match value with
  | CmapInt8 _ ->
      get_int8 handle key
  | CmapUInt8 _ ->
      get_uint8 handle key
  | CmapInt16 _ ->
      get_int16 handle key
  | CmapUInt16 _ ->
      get_uint16 handle key
  | CmapInt32 _ ->
      get_int32 handle key
  | CmapUInt32 _ ->
      get_uint32 handle key
  | CmapInt64 _ ->
      get_int64 handle key
  | CmapUInt64 _ ->
      get_uint64 handle key
  | CmapFloat _ ->
      get_float handle key
  | CmapDouble _ ->
      get_double handle key
  | CmapString _ ->
      get_string handle key
  | CmapBinary _ ->
      failwith "unimplemented" >>= Result.ok

let get_by_type val_type handle key =
  match val_type with
  | 1 ->
      get_int8 handle key >>= fun v -> string_of_int v |> Result.ok
  | 2 ->
      get_uint8 handle key >>= fun v -> Unsigned.UInt8.to_string v |> Result.ok
  | 3 ->
      get_int16 handle key >>= fun v -> string_of_int v |> Result.ok
  | 4 ->
      get_uint16 handle key >>= fun v ->
      Unsigned.UInt16.to_string v |> Result.ok
  | 5 ->
      get_int32 handle key >>= fun v -> Int32.to_string v |> Result.ok
  | 6 ->
      get_uint32 handle key >>= fun v ->
      Unsigned.UInt32.to_string v |> Result.ok
  | 7 ->
      get_int64 handle key >>= fun v -> Int64.to_string v |> Result.ok
  | 8 ->
      get_uint64 handle key >>= fun v ->
      Unsigned.UInt64.to_string v |> Result.ok
  | 9 ->
      get_float handle key >>= fun v -> Float.to_string v |> Result.ok
  | 10 ->
      get_double handle key >>= fun v -> Float.to_string v |> Result.ok
  | 11 ->
      get_string handle key >>= Result.ok
  | 12 ->
      failwith "get CmapBinary Unimplemented"
  | n ->
      raise (Unsupported_Valuetype n)

let get : type a. a value -> handle -> string -> (a, CsError.t) Result.t =
 fun v handle key ->
  let value_len = allocate size_t Unsigned.UInt64.zero in
  let value_type = allocate cmap_value_types_t 0 in
  cmap_get handle key null value_len value_type |> CsError.to_result
  >>= fun () ->
  if CmapValue.to_int v != !@value_type then
    Printf.sprintf "Get type mismatch: input %d, corosync %d"
      (CmapValue.to_int v) !@value_type
    |> failwith ;
  get_val v handle key

let set_int8 handle key value = cmap_set_int8 handle key value |> to_result

let set_uint8 handle key value = cmap_set_uint8 handle key value |> to_result

let set_int16 handle key value = cmap_set_int16 handle key value |> to_result

let set_uint16 handle key value = cmap_set_uint16 handle key value |> to_result

let set_int32 handle key value = cmap_set_int32 handle key value |> to_result

let set_uint32 handle key value = cmap_set_uint32 handle key value |> to_result

let set_int64 handle key value = cmap_set_int64 handle key value |> to_result

let set_uint64 handle key value = cmap_set_uint64 handle key value |> to_result

let set_string handle key value = cmap_set_string handle key value |> to_result

let set_float handle key value = cmap_set_float handle key value |> to_result

let set_double handle key value = cmap_set_double handle key value |> to_result

let set_by_type :
    type a. handle -> string -> a value -> (unit, CsError.t) result =
 fun handle key value ->
  match value with
  | CmapInt8 v ->
      set_int8 handle key v
  | CmapUInt8 v ->
      set_uint8 handle key v
  | CmapInt16 v ->
      set_int16 handle key v
  | CmapUInt16 v ->
      set_uint16 handle key v
  | CmapInt32 v ->
      set_int32 handle key v
  | CmapUInt32 v ->
      set_uint32 handle key v
  | CmapInt64 v ->
      set_int64 handle key v
  | CmapUInt64 v ->
      set_uint64 handle key v
  | CmapFloat v ->
      set_float handle key v
  | CmapDouble v ->
      set_double handle key v
  | CmapString v ->
      set_string handle key v
  | CmapBinary _ ->
      failwith "set binary Unimplemented"

let set handle key value = set_by_type handle key value

let rec get_prefix_rec handle prefix iter_handle =
  let value_len = allocate size_t Unsigned.UInt64.zero in
  let value_type = allocate cmap_value_types_t 0 in
  let key_arr = CArray.make char 256 in
  let key = CArray.start key_arr in
  match cmap_iter_next handle iter_handle key value_len value_type with
  | CsOk ->
      let key_name = Ctypes_std_views.string_of_char_ptr key in
      (* string_of_char_ptr will allocate a new string, so from this point on
         it is safe to collect key_arr and key *)
      Ctypes_memory_stubs.use_value (key_arr, key) ;
      get_by_type !@value_type handle key_name >>= fun hd ->
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
