(** [is_acro min_caps word] returns a bool indicating whether there are at
    least [min_caps] capital letters in [word]. *)
let is_acro ?(min_caps = 2) word =
    let f acc c = acc + (if Char.lowercase_ascii c <> c then 1 else 0) in
    let n_caps = String.fold_left f 0 word in
    n_caps >= min_caps
;;


(* Ideally we would use newtypes for row and column. I don't know how to do that
 * yet but see https://www.reddit.com/r/ocaml/comments/fez9om *)
type location = {
    file : string;
    line : int;
    column : int;
}

type acronym = {
    acronym : string;
    locations: location list;
}


let show_acronym acro : string = 
    let loc_info = acro.locations
     |> List.map (fun l -> String.concat ":" [l.file; string_of_int l.line; string_of_int l.column])
     |> List.map (String.cat "\n    ")
     |> String.concat "" in
    acro.acronym ^ loc_info
;;


let is_word_char = function
 | ' ' -> false
 | ',' -> false
 | '.' -> false
 | _   -> true
;;

(* chars_rev_to_string : char list -> string *)
let chars_rev_to_string chars =
    chars |> List.rev |> List.to_seq |> String.of_seq
;;


let split_words_with_col sentence : (string * int) list = 
    let third (_, _, x) = x in
    (* f : (int * char list * ((string * int) list)) -> char -> (string * int) list *)
    let f tuple c =  (* can you use pattern matching here? *)
        let current_col, current_word, words = tuple in
        let () = print_endline (current_word |> chars_rev_to_string) in
        match is_word_char c, current_word, words with
        (* no-op *)
        | false, [], words -> (current_col + 1, [], words)
        (* end of a new word *)
        | false, _, words -> let w = current_word |> chars_rev_to_string in
                              (current_col + 1,
                               [],
                               (w, current_col - String.length w) :: words)
        (* start or continuation of an old word *)
        | true, _, words  -> (current_col + 1, c :: current_word, words)
    in third (String.fold_left f (1, [], []) sentence)
;;


let find_acro ?(min_caps = 2) ~file ~line sentence : acronym list = 
    let words_with_cols = split_words_with_col sentence in
    let f (word_col : string * int) acros =
        let word, col = word_col in
        if is_acro ~min_caps word then
            let acro = { acronym = word ;
                         locations = [{
                             file   = file ;
                             line   = line ;
                             column = col}]
                         }
            in acro :: acros  (* TODO: join acronyms that are the same *)
        else acros
    in
    List.fold_right f words_with_cols []
;;
