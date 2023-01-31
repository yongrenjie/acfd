open OUnit2;;
open Acfd;;


let test_is_acro = 
    "is_acro" >:::
        [ "hello" >:: (fun _ -> assert_equal false (FindAcro.is_acro "hello" 2));
          "NMR" >:: (fun _ -> assert_equal true (FindAcro.is_acro "NMR" 2));
        ]
;;

let () =
    run_test_tt_main test_is_acro
;;
