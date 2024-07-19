open Ctypes
open Corosync_lib
open Corotypes
open CsError

let ( >>= ) = Result.bind

type name_format = AddressFormatName | AddressFormatIP

module ViewList = struct
  let mu = Mutex.create ()

  let g_view_list_entries = ref 0

  type node_name = Name of string | Ips of Ipaddr.t list

  type view_list_entry = {
      vq_info: Votequorum.vinfo option
    ; name: node_name option (* either the hostname or the ipaddrs *)
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

  let get_name_by_format node_id = function
    | AddressFormatIP -> (
        let open Cmap in
        with_handle @@ fun chandle ->
        get_prefix chandle "nodelist.node" >>= fun node_list ->
        List.find_opt (fun (_k, v) -> v = string_of_int node_id) node_list
        |> Option.fold ~none:(Error CsErrNotExist) ~some:(fun (name_key, _v) ->
               Scanf.sscanf name_key "nodelist.node.%d.nodeid" Fun.id
               |> Result.ok
           )
        >>= fun index ->
        List.filter_map
          (fun (k, v) ->
            if
              Astring.String.is_prefix
                ~affix:(Printf.sprintf "nodelist.node.%d.ring" index)
                k
            then
              Some v
            else
              None
          )
          node_list
        |> fun l ->
        (try List.map Ipaddr.of_string_exn l with Ipaddr.Parse_error _ -> [])
        |> function
        | [] ->
            Error CsErrExist
        | ipaddrs ->
            Ok (Ips ipaddrs)
        (* let open Cfg in
           cfg_get_node_addrs handle node_id >>= fun node_addrs ->
           Ok (List.map (fun {addr; _} -> addr) node_addrs |> String.concat ",") *)
      )
    | AddressFormatName ->
        let open Cmap in
        with_handle @@ fun chandle ->
        get_prefix chandle "nodelist.node" >>= fun node_list ->
        List.find_opt (fun (_k, v) -> v = string_of_int node_id) node_list
        |> Option.fold ~none:(Error CsErrNotExist) ~some:(fun (name_key, _v) ->
               Scanf.sscanf name_key "nodelist.node.%d.nodeid" Fun.id
               |> Result.ok
           )
        >>= fun id ->
        Printf.sprintf "nodelist.node.%d.name" id |> fun k ->
        List.assoc_opt k node_list
        |> Option.fold ~none:(Error CsErrExist) ~some:(fun name ->
               Ok (Name name)
           )

  let resolve_view_list_names format =
    let open Cfg in
    Mutex.lock mu ;
    let new_view_list =
      List.map
        (fun {node_id; _} ->
          get_name_by_format node_id format >>= fun addr ->
          Ok {node_id; name= Some addr; vq_info= None}
        )
        !g_view_list
    in
    match List.find_opt Result.is_error new_view_list with
    | Some e ->
        Result.get_error e |> Result.error
    | None ->
        Ok (List.map Result.get_ok new_view_list) >>= fun new_list ->
        g_view_list := new_list ;
        Ok () >>= fun () -> Mutex.unlock mu ; Ok ()

  let ocaml_quorum_notify_fn _handle _quorate _ring_seq view_list_entries
      view_list =
    Mutex.lock mu ;
    g_called := true ;
    g_view_list_entries := Unsigned.UInt32.to_int view_list_entries ;
    copy_view_list view_list !g_view_list_entries ;
    Mutex.unlock mu

  (** A view_list contains the current members in the quorum. Call this function
  to retrieve it. Call [update_membership_info] to update the viewlist before 
  retrieving view_list *)
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

(*
  The output of corosync-quorumtool can be separated into three sections: 
  1. Quorum information
  2. Votequorum information
  3. Membership information

  The second and third part are present only if votequorum(5) is enabled in corosync.

*)

let with_quorum_track f =
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
  let r = f () in
  quorum_trackstop !@qhandle |> CsError.to_result >>= fun () ->
  quorum_finalize !@qhandle |> to_result >>= fun () -> r

let is_quorate () = Quorum.(with_handle @@ getquorate)

let using_votequorum () =
  Cmapctl.get "quorum.provider"
  |> Result.fold
       ~ok:(String.equal "corosync_votequorum")
       ~error:(Fun.const false)

let votequorum_info id =
  if using_votequorum () then
    Votequorum.(with_handle @@ fun handle -> get_info handle id)
  else
    Error CsErrNoVoteQuorum

let my_votequorum_info () = Cfg.(with_handle cfg_local_get) >>= votequorum_info

let quorum_members name_format =
  with_quorum_track @@ fun () ->
  let open ViewList in
  ViewList.resolve_view_list_names name_format >>= fun () ->
  ViewList.get_view_list () |> Result.ok >>= fun vl ->
  let vqinfo_l = List.map (fun {node_id; _} -> votequorum_info node_id) vl in
  ( match List.find_opt Result.is_error vqinfo_l with
  | Some e ->
      Result.get_error e |> Result.error
  | None ->
      Ok (List.map Result.get_ok vqinfo_l)
  )
  >>= fun vqinfo_l ->
  List.map2 (fun vq_info vle -> {vle with vq_info= Some vq_info}) vqinfo_l vl
  |> Result.ok
