(** [is_acro min_caps word] returns a bool indicating whether there are at
    least [min_caps] capital letters in [word]. *)
let is_acro ?(min_caps = 2) word =
  let f acc c = acc + if Char.lowercase_ascii c <> c then 1 else 0 in
  let n_caps = String.fold_left f 0 word in
  n_caps >= min_caps

module FileMap = Map.Make (String)
module AcroMap = Map.Make (String)

let n_locations file_map =
  FileMap.fold (fun _ locs acc -> acc + List.length locs) file_map 0

(* Ideally we would use newtypes for row and column. I don't know how to do that
 * yet but see https://www.reddit.com/r/ocaml/comments/fez9om *)
type location = { file : string; line : int; column : int }

(** Pretty-print an acronym into a string. *)
let show_acronym acro locations : string =
  let show_locations (locs : (int * int) list) =
    locs
    |> List.map (fun t -> string_of_int (fst t) ^ ":" ^ string_of_int (snd t))
    |> String.concat ","
  in
  let loc_info =
    locations |> FileMap.map show_locations |> fun m ->
    FileMap.fold (fun k loc_str s -> s ^ "\n    " ^ k ^ ":" ^ loc_str) m ""
  in
  acro ^ loc_info

(** Pretty-print a set of acronyms on stdout. *)
let print_acronyms (acros : (int * int) list FileMap.t AcroMap.t) =
  AcroMap.iter (fun a l -> print_endline (show_acronym a l)) acros

(** [is_word_char] determines whether a character is part of a word. As of now,
    \[A-Za-z\] and hyphens are considered part of a word. This is somewhat
    arbitrary, and doesn't work for accented characters, but works fine for this
    project. *)
let is_word_char c =
  let open Char in
  c = '-' || (code c >= 65 && code c <= 90) || (code c >= 97 && code c <= 122)

(** Reverses a list of characters and constructs a string from them. *)
let chars_rev_to_string chars =
  chars |> List.rev |> List.to_seq |> String.of_seq

(** Splits words in a given sentence. Returns a list of [(word, col)] tuples,
    where [col] is the column number where the word begins. **)
let split_words_with_col sentence : (string * int) list =
  let third (_, _, x) = x in
  (* f : (int * char list * ((string * int) list)) -> char -> (string * int) list *)
  let f (current_col, current_word, words) c =
    match (is_word_char c, current_word, words) with
    (* no-op *)
    | false, [], words -> (current_col + 1, [], words)
    (* end of a new word *)
    | false, _, words ->
        let w = current_word |> chars_rev_to_string in
        (current_col + 1, [], (w, current_col - String.length w) :: words)
    (* start of a new word, or continuation of an old word *)
    | true, _, words -> (current_col + 1, c :: current_word, words)
  in
  third (String.fold_left f (1, [], []) sentence)

(** Insert an element [x] into a list [xs] which is sorted in ascending order,
    preserving the ordering of the list. *)
let rec insert_with_sort x xs =
  match xs with
  | [] -> [ x ]
  | y :: ys -> if x < y then x :: xs else y :: insert_with_sort x ys

(** Adds a new acronym (defined by [word, file, line, col]) to the list of
    acronyms tabulated so far. *)
let join_acro word file line col (acros : (int * int) list FileMap.t AcroMap.t)
    =
  (* Check if the existing acronym map contains the word already. *)
  match AcroMap.find_opt word acros with
  | None ->
      (* Not found; just add it in. *)
      let fm = FileMap.singleton file [ (line, col) ] in
      AcroMap.add word fm acros
  | Some fm -> (
      (* Found; need to merge the existing location lists. *)
      match FileMap.find_opt file fm with
      | None ->
          let fm' = FileMap.add file [ (line, col) ] fm in
          AcroMap.add word fm' acros
      | Some locs ->
          let fm' = FileMap.add file (insert_with_sort (line, col) locs) fm in
          AcroMap.add word fm' acros)

(** Extracts a list of acronyms from a given sentence. *)
let find_acro ?(min_caps = 2) file line acros sentence =
  let words_with_cols = split_words_with_col sentence in
  let f (word_col : string * int) acros =
    let word, col = word_col in
    if is_acro ~min_caps word then join_acro word file line col acros else acros
  in
  List.fold_right f words_with_cols acros

(** Read from a text file into a list of lines *)
let read_lines filename =
  In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'

(** Extracts a list of acronyms from a given file. *)
let find_acros_in_file acros filename =
  let f (s, i) acros = find_acro filename i acros s in
  let line_number_ts =
    filename |> read_lines |> List.mapi (fun i s -> (s, i + 1))
  in
  List.fold_right f line_number_ts acros

(** Extracts a list of acronyms from a list of files. *)
let find_acros_in_files acros filenames =
  List.fold_right (Fun.flip find_acros_in_file) filenames acros

(** Get a list of .tex files in a directory and any subdirectories. *)
let rec get_tex_files dir =
  let open Sys in
  let fs =
    dir |> readdir |> Array.to_list |> List.map (fun f -> dir ^ "/" ^ f)
  in
  let thisdir_tex = fs |> List.filter (String.ends_with ~suffix:".tex") in
  let subdir_tex =
    fs |> List.filter Sys.is_directory |> List.concat_map get_tex_files
  in
  thisdir_tex @ subdir_tex

(** Extract all acronyms from a LaTeX project. [dir] should be the root of the
    project. *)
let find_acros_in_dir dir =
  find_acros_in_files AcroMap.empty (get_tex_files dir)
