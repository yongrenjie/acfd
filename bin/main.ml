open Acfd;;


let acronyms = FindAcro.find_acro ~min_caps:3 "NMR includes experiments such as COSY, HSQC, and HMBC."

let () = acronyms |> List.map FindAcro.show_acronym |> List.iter print_endline
;;
