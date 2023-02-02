open Acfd

let () =
  Sys.getenv "HOME" ^ "/thesis" |> Find.find_acros_in_dir |> Find.print_acronyms
