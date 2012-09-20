
let usage = "Usage: "^Sys.argv.(0)^" <input options> [-o output]\n\
             Translate programs to the IL. "

let out = ref stdout
let speclist =
  ("-o", Arg.String (fun f -> out := open_out f),
   "<file> Print output to <file> rather than stdout.")
    :: Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage


let prog =
  try fst (Input.get_program())
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let pp = new Pp.pp_oc !out
;;

pp#ast_program prog;
pp#close;
