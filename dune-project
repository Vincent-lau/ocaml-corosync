(lang dune 3.11)

(using ctypes 0.3)

(name corosync)

(generate_opam_files true)

(source
 (github Vincent-lau/ocaml-corosync))

(authors "Shuntian Liu")

(maintainers "Shuntian Liu")

(license LGPL-2.1-only)

(documentation https://url/to/documentation)

(package
 (name corosync)
 (synopsis "OCaml Corosync binding")
 (description "An OCaml language binding to libcorosync")
 (depends
  ocaml
  dune
  stdint
  ctypes
  ctypes-foreign
  (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
