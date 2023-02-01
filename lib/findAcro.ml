(** [is_acro word min_caps] returns a bool indicating whether there are at
    least [min_caps] capital letters in [word]. *)
let is_acro word min_caps =
    let acc_caps acc c = acc + (if Char.lowercase_ascii c <> c then 1 else 0) in
    let n_caps = String.fold_left acc_caps 0 word in
    n_caps > min_caps
;;
