(** Use this to read in a program.

    TODO: Add convenience functions to get SSA directly, and maybe more input
    options.
*)

open Arg
open Grammar_scope

(** A speclist suitable to pass to Arg.parse.
    Add this to the speclist for your program. *)
val speclist : (key * spec * doc) list

val stream_speclist : (key * spec * doc) list

(** Get the program as specified by the commandline. *)
val get_program : unit -> Ast.program * Scope.t

val get_stream_program : unit -> (Ast.program) Stream.t

val init_ro : bool ref

(* This really should go elsewhere! *)
val set_gc : unit -> unit
