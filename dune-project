(lang dune 3.11)

(using ctypes 0.3)

(name ocaml-corosync)

(generate_opam_files true)

(source
 (github Vincent-lau/ocaml-corosync))

(authors "Shuntian Liu")

(maintainers "Shuntian Liu")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml-corosync)
 (synopsis "OCaml Corosync binding")
 (description "An OCaml language binding to libcorosync")
 (depends ocaml dune stdint alcotest :with-test)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
