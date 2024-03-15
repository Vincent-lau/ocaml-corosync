val cmap_handle_t : Unsigned.uint64 Ctypes_static.typ

val size_t : Unsigned.uint64 Ctypes_static.typ

val cmap_value_types_t : int Ctypes_static.typ

val cs_error_t : int Ctypes_static.typ

module CmapError : sig
  exception Unknown_Err_Code of int

  type t =
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
    | CsErrAccess
    | CsErrNotExist
    | CsErrNameTooLong
    | CsErrExist
    | CsErrNoSpace
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
    | CsErrTooBig
    | CsErrNoSections
    | CsErrContextNotFound
    | CsErrTooManyGroups
    | CsErrSecurity

  val to_result : int -> (unit, t) result

  val from_int : int -> t

  val to_string : t -> string
end

module CmapValuetype : sig
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

  val from_int : int -> t
end

module CmapRet : sig
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
    | CmapDouble of float
    | CmapString of string
    | CmapBinary of bytes

  val to_string : t -> string
end

val get : Unsigned.uint64 -> string -> (CmapRet.t, CmapError.t) result

val get_prefix :
  Unsigned.uint64 -> string -> ((string * CmapRet.t) list, CmapError.t) result

val with_handle :
  (Unsigned.uint64 -> ('a, CmapError.t) result) -> ('a, CmapError.t) result
