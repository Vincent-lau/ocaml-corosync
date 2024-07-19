## v0.0.1

- Initial public release of ocaml corosync binding
- Contains bindings to
  - libcfg
  - libcmap
  - libquorum
  - libvotequorum
  - and various helper functions to mimic the corosync tool binaries
- Added test cases for
  - when corosync is running
  - and when it is not (the default)
- Written using ctypes!
