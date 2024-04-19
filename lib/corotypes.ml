open Ctypes

module CsError = struct
  exception Unknown_Err_Code of int

  let cs_error_t = int

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
    | CsErrNoVoteQuorum (* 998 *)
    | CsErrOcamlCompat (* 999 *)

  let from_int = function
    | 1 ->
        CsOk
    | 2 ->
        CsErrLibrary
    | 3 ->
        CsErrVersion
    | 4 ->
        CsErrInit
    | 5 ->
        CsErrTimeout
    | 6 ->
        CsErrTryAgain
    | 7 ->
        CsErrInvalidParam
    | 8 ->
        CsErrNoMemory
    | 9 ->
        CsErrBadHandle
    | 10 ->
        CsErrBusy
    | 11 ->
        CsErrExist
    | 12 ->
        CsErrNotExist
    | 13 ->
        CsErrNameTooLong
    | 14 ->
        CsErrExist
    | 15 ->
        CsErrNoSpace
    | 16 ->
        CsErrInterupt
    | 17 ->
        CsErrNameNotFound
    | 18 ->
        CsErrNoResources
    | 19 ->
        CsErrNotSupported
    | 20 ->
        CsErrBadOperation
    | 21 ->
        CsErrFailedOperation
    | 22 ->
        CsErrMessageError
    | 23 ->
        CsErrQueueFull
    | 24 ->
        CsErrQueueNotAvailable
    | 25 ->
        CsErrBadFlags
    | 26 ->
        CsErrTooBig
    | 27 ->
        CsErrNoSections
    | 28 ->
        CsErrContextNotFound
    | 30 ->
        CsErrTooManyGroups
    | 100 ->
        CsErrSecurity
    | 999 ->
        CsErrOcamlCompat
    | e ->
        raise (Unknown_Err_Code e)

  let to_result n = from_int n |> function CsOk -> Ok () | e -> Error e

  (* TODO finish this *)
  let to_string = function
    | CsOk ->
        "cs_ok"
    | CsErrNoSections ->
        "cs_err_no_sections"
    | CsErrLibrary ->
        "cs_err_library"
    | CsErrOcamlCompat ->
        "unknown return value from corosync, ocaml cannot parse"
    | _ ->
        failwith "cserror to string Unimplemented"
end

module CsDispatchFlag = struct
  exception Unknown_Dispatch_Flag of int

  type t =
    | CsDispatchOne
    | CsDispatchAll
    | CsDispatchBlocking
    | CsDispatchOneNonBlocking

  let cs_dispatch_flags_t = int

  let from_int = function
    | 1 ->
        CsDispatchOne
    | 2 ->
        CsDispatchAll
    | 3 ->
        CsDispatchBlocking
    | 4 ->
        CsDispatchOneNonBlocking
    | n ->
        raise (Unknown_Dispatch_Flag n)

  let to_int = function
    | CsDispatchOne ->
        1
    | CsDispatchAll ->
        2
    | CsDispatchBlocking ->
        3
    | CsDispatchOneNonBlocking ->
        4
end

let cs_track_current = 0x1

let cs_track_changes = 0x2

let cs_track_changes_only = 0x3

let interface_max = 8
