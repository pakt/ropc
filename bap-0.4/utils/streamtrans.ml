let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "

(* open Bap*)

type ast = Ast.program
(*type astcfg = Cfg.AST.G.t
type ssa = Cfg.SSA.G.t*)

type prog =
  | Ast of ast

type cmd = 
  | TransformAst of (ast -> ast)

let concrete_state = Traces.TraceConcrete.create_state ()

let pipeline = ref []

let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

(** Prints the block *)
let prints block =
  Printf.printf "new block\n";
  List.iter
    (fun stmt ->
       Printf.printf "Stmt: %s\n" (Pp.ast_stmt_to_string stmt)
    ) block;
  Printf.printf "end block\n";
  block

(** Concretely executes a block *)
let concrete block =
  let no_specials = Traces.remove_specials block in
  (* SWXXX use disasm.mem(?) instead *)
  let memv = Traces.find_memv no_specials in
  let trace = 
	try
	  (* SWXXX modify to use range (should look a lot like loop in traces.ml) *)
	  (* SWXXX do memory2array at block level *)
	  Traces.run_block concrete_state memv no_specials None
	with Failure "empty list" -> (*Printf.printf "run blocks failed\n";*) [] in
  trace

let speclist =
  ("-print", uadd(TransformAst(prints)),
     "Print each statement in the trace.")
  ::("-concrete", uadd(TransformAst(concrete)),
     "Concretely execute each block.")
  :: Input.stream_speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline

let prog =
  try Input.get_stream_program ()
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let rec apply_cmd prog = function
  | TransformAst f -> (
      match prog with
      | Ast p -> Ast(f p)
    )
;;

Stream.iter
  (fun block ->
     ignore(List.fold_left apply_cmd (Ast block) pipeline)
  ) prog


