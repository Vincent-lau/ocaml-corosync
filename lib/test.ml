open Ctypes
open PosixTypes
open Foreign


let time = foreign "time" (ptr time_t @-> returning time_t)

let difftime = foreign "difftime" (time_t @-> time_t @-> returning double)

let time' () = time (from_voidp time_t null)

(* val measure_execution_time : (unit -> unit) -> float *)
let measure_execution_time timed_function =
  let start_time = time' () in
  let () = timed_function () in
  let end_time = time' () in
  difftime end_time start_time


