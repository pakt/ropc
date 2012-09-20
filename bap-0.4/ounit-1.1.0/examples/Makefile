# 
# 

TESTS = test_list.ml test_list2.ml test_stack.ml

test: test_suite
	./test_suite

test_suite: $(TESTS) test_suite.ml
	ocamlfind ocamlc -o test_suite -package oUnit -linkpkg \
	test_list.ml test_list2.ml test_stack.ml test_suite.ml	
