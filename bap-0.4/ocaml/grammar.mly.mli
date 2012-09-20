(* grammar.mly.mli
This is a fake .mli file to be appended to that produced by ocamlyacc
There doesn't seem to be a good way to say what gets exposed from the
.mly file, so I cobbled this together.
Of course, this requires some hacking of the build system...
 --Ivan
*)


type scope

val scope_create : Var.t list -> scope
let scope_set : scope -> unit
let scope_default : scope
