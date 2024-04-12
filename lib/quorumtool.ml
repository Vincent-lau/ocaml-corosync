open Ctypes
open Corotypes
open CsError

let ( >>= ) = Result.bind

module ViewList = struct
  let mu = Mutex.create ()

  let g_view_list_entries = ref 0

  type view_list_entry = {
      vq_info: Votequorum.vinfo option
    ; name: string option
    ; node_id: int
  }

  let g_view_list : view_list_entry list ref = ref []

  let g_called = ref false

  let copy_view_list view_list view_list_entries =
    List.init view_list_entries Fun.id
    |> List.map (fun i ->
           let node_id = !@(view_list +@ i) |> Unsigned.UInt32.to_int in
           {vq_info= None; name= None; node_id}
       )
    |> ( := ) g_view_list

  let resolve_view_list_names () =
    let open Cfg in
    Mutex.lock mu ;
    let new_view_list =
      List.map
        (fun {node_id; _} ->
          with_handle @@ fun handle ->
          cfg_get_node_addrs handle node_id >>= fun node_addrs ->
          List.map (fun {addr; _} -> addr) node_addrs |> String.concat ","
          |> fun addr -> Ok {node_id; name= Some addr; vq_info= None}
        )
        !g_view_list
    in
    List.find_opt Result.is_error new_view_list
    |> Option.fold
         ~some:(fun e ->
           let e = Result.get_error e in
           Error e
         )
         ~none:(Ok (List.map Result.get_ok new_view_list))
    >>= fun new_list ->
    g_view_list := new_list ;
    Ok () >>= fun () -> Mutex.unlock mu ; Ok ()

  let ocaml_quorum_notify_fn _handle _quorate _ring_seq view_list_entries
      view_list =
    Mutex.lock mu ;
    g_called := true ;
    g_view_list_entries := Unsigned.UInt32.to_int view_list_entries ;
    copy_view_list view_list !g_view_list_entries ;
    Mutex.unlock mu

  let get_view_list_entries () =
    let r = ref 0 in
    Mutex.lock mu ;
    r := !g_view_list_entries ;
    Mutex.unlock mu ;
    !r

  let get_view_list () =
    let r : view_list_entry list ref = ref [] in
    Mutex.lock mu ;
    r := !g_view_list ;
    Mutex.unlock mu ;
    !r

  let get_g_called () =
    let r = ref false in
    Mutex.lock mu ;
    r := !g_called ;
    Mutex.unlock mu ;
    !r

  let reset_g_called () =
    Mutex.lock mu ;
    g_called := false ;
    Mutex.unlock mu
end

let dispatch qhandle flag =
  ViewList.reset_g_called () ;
  let rec dispatch_aux qhandle flag =
    Quorum.quorum_dispatch qhandle flag |> CsError.to_result >>= fun () ->
    if ViewList.get_g_called () then
      Ok ()
    else
      dispatch_aux qhandle flag
  in
  dispatch_aux qhandle flag

let with_handle f =
  let open Quorum in
  let qhandle = allocate quorum_handle_t Unsigned.UInt64.zero in
  let qtype = allocate uint32_t Unsigned.UInt32.zero in
  let qcb = make quorum_callbacks_t in
  setf qcb quorum_notify_fn ViewList.ocaml_quorum_notify_fn ;
  quorum_initialize qhandle (addr qcb) qtype |> CsError.to_result >>= fun () ->
  quorum_trackstart !@qhandle (Unsigned.UInt.of_int cs_track_current)
  |> CsError.to_result
  >>= fun () ->
  dispatch !@qhandle CsDispatchFlag.(CsDispatchOne |> to_int) >>= fun () ->
  let r = f !@qhandle in
  quorum_trackstop !@qhandle |> CsError.to_result >>= fun () ->
  quorum_finalize !@qhandle |> to_result >>= fun () -> r
