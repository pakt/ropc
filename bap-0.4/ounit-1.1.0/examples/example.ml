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
let _ =
  let verbose = ref false in
  let set_verbose _ = verbose := true in
  Arg.parse
    [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    ("usage: " ^ Sys.argv.(0) ^ " [-verbose]");
  if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
    exit 1

