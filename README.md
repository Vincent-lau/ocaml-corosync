# OCaml Corosync binding

OCaml bindings to for libcorosync, currently including libquorum, libvotequorum,
libcfg and libcmap.

Operations require Corosync to be running.


## Installation

This library is published on opam, so one can do 

```
opam install corosync
```

to install the library

## Get started

To get started, make sure you have Corosync configured and running on your cluster.
If you are new to corosync, start by experimenting some of the builtin Corosync
cmd tools such as `corosync-cmapctl`, `corosync-quorumtool` etc. Then look at the 
tools library to see what high level functions are provided by this binding.
`bin/main.ml` also provides some example code that can be run to test things out.

## Example

Here are some example code of how you can use the binding. Please note, all of these
will require you to have set up a corosync cluster. For details on how to do that
please refer to the [corosync]() website.

The following code will query whether your current cluster is quorate:
```ocaml
open Corosync_tools

let _ = 
  let open Quorumtool in 
  match is_quorate () with
  | Ok quorate ->
    Printf.printf "is quorate %b\n" quorate
  | Error e -> 
    print_endline (Corosync_lib.Corotypes.CsError.to_string e)
```

And this code can tell you the name of your cluster. Note you will need to understand
the return type of each keys stored in the cmap database. See the documentation 
of `Cmapctl.get` for more details

```ocaml
open Corosync_tools
open Corosync_lib

let _ =
  let open Cmap in
  match Cmapctl.get CmapValue.string "totem.cluster_name" with
  | Ok res -> Printf.printf "cluster name %s\n" res
  | Error e -> Corotypes.CsError.to_string e |> print_endline
```
