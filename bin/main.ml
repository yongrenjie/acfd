open Acfd
open Cmdliner

let show_dir dir verb =
  dir |> Find.find_acros_in_dir |> Find.print_acronyms verb

let dir_t =
  let doc = "The root of the LaTeX project" in
  Arg.(required & pos 0 (some string) None & info ~doc ~docv:"ROOT" [])

let verbose_t =
  let doc = "Control output detail" in
  Arg.(value & opt int 0 & info ~doc ~docv:"VERBOSITY" [ "v"; "verbose" ])

let t = Term.(const show_dir $ dir_t $ verbose_t)
let cmd = Cmd.v (Cmd.info "acfd") t
let () = exit (Cmd.eval cmd)
