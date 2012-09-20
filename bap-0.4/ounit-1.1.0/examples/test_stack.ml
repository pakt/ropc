open OUnit

(* 
 * This test shows how brackets can be used. They are handy to create
 * a so called fixture, which can be used for multiple tests 
 *)

(* prepare a stack for test *)
let setup _ =
  let s = Stack.create () in
  Stack.push 1 s;
  Stack.push 2 s;
  Stack.push 3 s;
  s

let teardown _ = 
  ()

let test_top stack =
  assert_equal 3 (Stack.top stack)

let test_clear stack =
  Stack.clear stack;
  assert_raises Stack.Empty (fun _ -> Stack.top stack)

let test_pop stack = 
  assert_equal 3 (Stack.pop stack);
  assert_equal 2 (Stack.pop stack);
  assert_equal 1 (Stack.pop stack);
  assert_raises Stack.Empty (fun _ -> Stack.pop stack)

let suite = "Test Stack" >::: 
  ["test_top" >:: (bracket setup test_top teardown);
   "test_clear" >:: (bracket setup test_clear teardown);
   "test_pop" >:: (bracket setup test_pop teardown)]


