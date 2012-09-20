(***********************************************************************)
(* The OUnit library                                                   *)
(*                                                                     *)
(* Copyright 2002, 2003, 2004, 2005, 2006, 2007, 2008                  *)
(* Maas-Maarten Zeeman.                                                *)
(* Copyright 2010 OCamlCore SARL                                       *)
(* All rights reserved. See LICENCE for details.                       *)
(***********************************************************************)

open OUnit
  
let test_case = TestCase (fun () -> ())
let labeled_test_case = "label" >: test_case
let suite_a = "suite_a" >: TestList [test_case]
let suite_b = "suite_b" >: TestList [labeled_test_case]
let suite_c = "suite_c" >: TestList [test_case; labeled_test_case]
let suite_d = "suite_d" >: TestList [suite_a; suite_c]  

let rec string_of_paths = function
    [] -> ""
  | h::t -> (string_of_path h) ^ "\n" ^ (string_of_paths t)

(* Test which checks if the test case count function works correctly *)
let test_case_count _ =
  let assert_equal ?msg = assert_equal ?msg ~printer:string_of_int in
  assert_equal 0 (test_case_count (TestList []));
  assert_equal 0 (test_case_count (TestLabel("label", TestList [])));
  assert_equal 0 
    (test_case_count 
       (TestList [TestList []; 
                  TestList [TestList []]]));

  assert_equal 1 (test_case_count test_case);
  assert_equal 1 (test_case_count labeled_test_case);
  assert_equal 1 (test_case_count suite_a);
  assert_equal 1 (test_case_count suite_b);
  
  assert_equal 1 (test_case_count (TestList [suite_a; TestList []]));
  assert_equal 1 
    (test_case_count 
       (TestList [TestList []; 
                  TestList [suite_b]]));
  assert_equal 2 (test_case_count suite_c);
  assert_equal 3 (test_case_count suite_d)

(* Test which checks if the paths are correctly constructed *)
let test_case_paths _ =
      (* A single testcase results in a list countaining an empty list *)
  let assert_equal ?msg = assert_equal ?msg ~printer:string_of_paths in
  assert_equal [[]] (test_case_paths test_case);
  assert_equal [[Label "label"]] 
    (test_case_paths labeled_test_case);
  assert_equal [[ListItem 0; Label "suite_a"]] 
    (test_case_paths suite_a);
  assert_equal [[Label "label"; ListItem 0; Label "suite_b"]] 
    (test_case_paths suite_b);
  assert_equal [[ListItem 0; Label "suite_c"]; 
                [Label "label"; ListItem 1; Label "suite_c"]] 
    (test_case_paths suite_c);
  assert_equal [[ListItem 0; Label "suite_a"; ListItem 0; Label "suite_d"];
                [ListItem 0; Label "suite_c"; ListItem 1; Label "suite_d"];
                [Label "label"; ListItem 1; Label "suite_c"; ListItem 1;
                 Label "suite_d"]] 
    (test_case_paths suite_d)

let test_assert_raises _ =
  assert_raises 
    (Failure "OUnit: expected: Failure(\"Boo\") but got: Failure(\"Foo\")") 
    (fun _ -> (assert_raises (Failure "Boo") 
                 (fun _ -> raise (Failure "Foo"))));
  assert_raises 
    (Failure "OUnit: A label\nexpected: Failure(\"Boo\") but got: Failure(\"Foo\")") 
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo") 
                 (fun _ -> raise (Failure "Foo"))));
  assert_raises 
    (Failure "OUnit: expected exception Failure(\"Boo\"), but no exception was raised.") 
    (fun _ -> (assert_raises (Failure "Boo") (fun _ -> ())));
  assert_raises 
    (Failure "OUnit: A label\nexpected exception Failure(\"Boo\"), but no exception was raised.") 
    (fun _ -> (assert_raises ~msg:"A label" (Failure "Boo") (fun _ -> ())))

(* Test the float compare, and use the cmp label *)
let test_cmp_float _ =
  assert_equal ~cmp: cmp_float 0.0001 0.0001;
  assert_equal ~cmp: (cmp_float ~epsilon: 0.001) 1.0001 1.00001;
  assert_raises (Failure "OUnit: not equal") 
      (fun _ -> assert_equal ~cmp: cmp_float 100.0001 101.001)

let test_assert_string _ =
  assert_string "";
  assert_raises (Failure "OUnit: A string") 
    (fun _ -> assert_string "A string")

let test_assert_bool _ =
  assert_bool "true" true;
  assert_raises (Failure "OUnit: false") 
    (fun _ -> assert_bool "false" false)

let test_case_filter () = 
  let assert_test_case_count res tst_opt =
    match tst_opt with 
      | Some tst ->
          assert_equal res (OUnit.test_case_count tst)
      | None ->
          assert_failure "Unexpected empty filter result"
  in
  assert_equal None (test_filter [] suite_a);
  assert_equal None (test_filter [] suite_b);
  assert_equal None (test_filter [] suite_c);
  assert_equal None (test_filter [] suite_d);
  assert_test_case_count 1 (test_filter ["suite_a"] suite_a);  
  assert_test_case_count 1 (test_filter ["suite_a:0"] suite_a);  
  assert_test_case_count 1 (test_filter ["suite_b:0:label"] suite_b);  
  assert_test_case_count 1 (test_filter ["suite_c:0"] suite_c);
  assert_test_case_count 2 (test_filter ["suite_c:0";"suite_c:1:label"] suite_c) 

let assert_equal_test_result =
  assert_equal 
    ~printer:(fun tst_results -> 
                String.concat "; "
                  (List.map
                     (function 
                        | RSuccess path -> 
                            Printf.sprintf "RSuccess %S" (string_of_path path)
                        | RFailure (path, str) ->
                            Printf.sprintf "RFailure(%S, %S)" 
                              (string_of_path path)
                              str
                        | RError (path, str) ->
                            Printf.sprintf "RError(%S, %S)" 
                              (string_of_path path)
                              str
                        | RSkip (path, str) ->
                            Printf.sprintf "RSkip(%S, %S)" 
                              (string_of_path path)
                              str
                        | RTodo (path, str) ->
                            Printf.sprintf "RTodo(%S, %S)" 
                              (string_of_path path)
                              str
                     )
                     tst_results
                  ))

let test_case_decorate () = 
    assert_equal_test_result 
      [RSuccess [Label "label"; ListItem 1; Label "suite_c"]; 
       RSuccess [ListItem 0; Label "suite_c"]]
      (perform_test ignore suite_c);
    assert_equal_test_result
      [RFailure([Label "label"; ListItem 1; Label "suite_c"], "OUnit: fail"); 
       RFailure([ListItem 0; Label "suite_c"], "OUnit: fail")]
      (perform_test ignore 
         (test_decorate (fun _ -> (fun () -> assert_failure "fail")) suite_c))

let test_case_skip () = 
  assert_equal_test_result
    [RSkip ([Label "skip"], "test")] 
    (perform_test ignore ("skip" >:: (fun () -> skip_if true "test")))

let test_case_todo () = 
  assert_equal_test_result
    [RTodo ([Label "todo"], "test")] 
    (perform_test ignore ("todo" >:: (fun () -> todo "test")))

let test_assert_command () = 
  assert_command Sys.executable_name ["-help"]

module EInt = 
struct 
  type t = int
  let compare = ( - )
  let pp_printer = Format.pp_print_int
  let pp_print_sep = OUnitDiff.pp_comma_separator
end
                  

module DiffSetInt = OUnitDiff.SetMake(EInt)

module DiffListSimpleInt = OUnitDiff.ListSimpleMake(EInt)

let test_diff () = 
  let lst_exp = 
    [1; 2; 3; 4; 5]
  in
  let lst_real = 
    [1; 2; 5; 4]
  in
    assert_raises 
      (Failure "OUnit: expected: 1, 2, 3, 4, 5 but got: 1, 2, 4, 5\ndifferences: -3")
      (fun () ->
         DiffSetInt.assert_equal 
           (DiffSetInt.of_list lst_exp)
           (DiffSetInt.of_list lst_real));
    DiffSetInt.assert_equal (DiffSetInt.of_list lst_exp) (DiffSetInt.of_list lst_exp);
    assert_raises 
      (Failure "OUnit: expected: 1, 2, 3, 4, 5 but got: 1, 2, 5, 4\
                \ndifferences: element number 2 differ (3 <> 5)")
      (fun () ->
         DiffListSimpleInt.assert_equal lst_exp lst_real);
    DiffListSimpleInt.assert_equal lst_exp lst_exp


(* Construct the test suite *)
let suite = "OUnit" >::: 
  [ "test_case_count" >:: test_case_count;
    "test_case_paths" >:: test_case_paths;
    "test_assert_raises" >:: test_assert_raises;
    "test_assert_string" >:: test_assert_string;
    "test_assert_bool" >:: test_assert_bool;
    "test_cmp_float" >:: test_cmp_float;
    "test_case_filter" >:: test_case_filter;
    "test_case_decorate" >:: test_case_decorate;
    "test_case_skip" >:: test_case_skip;
    "test_case_todo" >:: test_case_todo;
    "test_assert_command" >:: test_assert_command;
    "test_diff" >:: test_diff;
  ]

(* Run the tests in test suite *)
let _ = run_test_tt_main suite
