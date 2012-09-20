open Ast
open Type

let usage = "Usage: "^Sys.argv.(0)^" input_file [output_prefix]\n\
             Split a disjunction into many formulas. "


let split_disjunction =
  let rec h fs = function
    | BinOp(OR, a, b) ->
	h (h fs a) b
    | f -> f::fs
  in
  h []


let filename = ref ""
let prefix = ref ""
let countonly = ref false

let speclist =
  ("-c", Arg.Set countonly,
   "Only count the number of disjunct expressions.")
  :: [] (*Input.speclist*)

let anon x =
  if !filename = "" then filename := x
  else if !prefix = "" then prefix := x
  else raise(Arg.Bad("Don't know what to do with "^x))
let () = Arg.parse speclist anon usage


let e,_ = Parser.exp_from_file !filename
let es = split_disjunction e

let write_file n e =
  let s = !prefix ^ string_of_int n in
  let pp = new Pp.pp_oc (open_out (s^".il")) in
  pp#ast_exp e;
  pp#close;
  let m2a = new Memory2array.memory2array_visitor () in
  let e = Ast_visitor.exp_accept m2a e in
  let p = new Stp.pp_oc (open_out (s^".stp")) in
  p#assert_ast_exp e;
  p#close

    
;;

if !countonly then print_int(List.length es)
else
  ignore(
    List.fold_left
      (fun n e ->
	 write_file n e;
	 succ n
      )
      0
      es
  )
