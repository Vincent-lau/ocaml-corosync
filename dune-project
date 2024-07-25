(lang dune 3.11)

(using ctypes 0.3)

(name corosync)

(generate_opam_files true)

(source
 (github Vincent-lau/ocaml-corosync))

(authors "Shuntian Liu")

(maintainers "Shuntian Liu")

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(documentation https://Vincent-lau.github.io/ocaml-corosync/doc)

(package
 (name corosync)
 (synopsis "OCaml Corosync binding")
 (description "An OCaml language binding to libcorosync")
 (depends
  (astring
   (>= 0.8.5))
  (ocaml (>= 4.08))
  (dune (>= 3.16))
  stdint
  (ctypes (>= 0.22.0))
  (ctypes-foreign (>= 0.22.0))
  ipaddr
  (alcotest (>= 1.7.0))
  conf-libcorosync
  )
 (tags
  (corosync binding)))