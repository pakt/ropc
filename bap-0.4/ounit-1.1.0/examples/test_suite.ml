open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "OUnit Example" >::: 
  [Test_list.suite; 
   Test_list2.suite;
   Test_stack.suite]

let _ =
  run_test_tt_main suite
