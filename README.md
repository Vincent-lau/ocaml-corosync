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
