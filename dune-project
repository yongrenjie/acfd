(lang dune 3.3)

(name acfd)

(generate_opam_files true)

(source
 (github yongrenjie/acfd))

(authors "Jonathan Yong")

(maintainers "Jonathan Yong")

(license MIT)

; (documentation https://url/to/documentation)

(package
 (name acfd)
 (synopsis "Acronym finder")
 (description "Search for (potential) acronyms in a LaTeX project")
 (depends ocaml dune))

; note that `dune init proj <NAME>` creates a spurious `_build` directory at
; the same level as the project directory. This will be fixed in dune 3.7.0
; (see https://github.com/ocaml/dune/pull/6707), but until then, the directory
; can be safely deleted.
