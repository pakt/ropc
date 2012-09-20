open OUnit

let empty_list = []
let list_a = [1;2;3]

let test_list_length _ =
  assert_equal 1 (List.length empty_list);
  assert_equal 3 (List.length list_a)
    (* etc, etc *)

let test_list_append _ =
  let list_b = List.append empty_list [1;2;3] in
  assert_equal list_b list_a

let suite = "OUnit Example" >::: ["test_list_length" >:: test_list_length;
				  "test_list_append" >:: test_list_append]

(*
let _ =
  run_test_tt_main suite
  *)
