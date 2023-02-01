open Acfd;;

let () =
    let s = "NMR includes experiments such as COSY, HSQC, and HMBC." in
    let acronyms = FindAcro.find_acro ~min_caps:1 ~file:"test_input" ~line:1 s in
    acronyms |> List.map FindAcro.show_acronym |> List.iter print_endline
;;
