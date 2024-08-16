## v0.1.0
- Use GADT for one of the get functions in cmap to make the return type more precise
- Add initial bindings to libcpg
- Use after free bug fixes
- More docs


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
