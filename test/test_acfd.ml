open OUnit2;;
open Acfd;;


let test_is_acro = 
    "is_acro" >:::
        [ "hello" >:: (fun _ -> assert_equal false (FindAcro.is_acro "hello"));
          "NMR" >:: (fun _ -> assert_equal true (FindAcro.is_acro "NMR"));
        ]
;;

let () =
    run_test_tt_main test_is_acro
;;
