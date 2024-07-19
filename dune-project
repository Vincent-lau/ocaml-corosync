(lang dune 3.11)

(using ctypes 0.3)

(name corosync)

(generate_opam_files true)

(source
 (github Vincent-lau/ocaml-corosync))

(authors "Shuntian Liu")

(maintainers "Shuntian Liu")

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(documentation https://url/to/documentation)

(package
 (name corosync)
 (synopsis "OCaml Corosync binding")
 (description "An OCaml language binding to libcorosync")
 (depends
  (astring
   (>= 0.8.5))
  ocaml
  dune
  stdint
  ctypes
  ctypes-foreign
  ipaddr
  alcotest)
 (tags
  (corosync binding)))