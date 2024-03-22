exception Unknown_Err_Code of int

val cs_error_t : int Ctypes_static.typ

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
  (* ocaml side error *)
  | CsErrOcamlCompat (* 999 *)

val from_int : int -> t

val to_result : int -> (unit, t) result

val to_string : t -> string
