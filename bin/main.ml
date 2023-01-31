open Acfd;;

let () = print_endline (
    if (FindAcro.is_acro "HSQC" 2)
    then "HSQC is an acronym (I think)"
    else "HSQC isn't an acronym for some reason"
    )
;;
