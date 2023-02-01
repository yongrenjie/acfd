(** [is_acro min_caps word] returns a bool indicating whether there are at
    least [min_caps] capital letters in [word]. *)
let is_acro ?(min_caps = 2) word =
    let acc_caps acc c = acc + (if Char.lowercase_ascii c <> c then 1 else 0) in
    let n_caps = String.fold_left acc_caps 0 word in
    n_caps >= min_caps
;;


type location = {
    file : string;
    row : int;
    column : int;
}

type acronym = {
    acronym : string;
    locations: location list;
}


let show_acronym acro : string = 
    "acro: " ^ acro.acronym
;;


let split_words sentence : string list = 
    String.split_on_char ' ' sentence
;;


let find_acro ?(min_caps = 2) sentence : acronym list = 
    let words = split_words sentence in
    let f acros word =
        if is_acro ~min_caps word then
            let acro = { acronym = word ; locations = [] }
            in acro :: acros
        else acros
    in
    List.fold_left f [] words
;;
