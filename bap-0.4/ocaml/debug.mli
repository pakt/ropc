(*
*  debug.mli
*
*  Made by (David Brumley)
*)

module type DEBUG =
  sig
    val debug : bool
    val warn : bool
    val pdebug : string -> unit
    val dprintf : ('a, unit, string, unit) format4 -> 'a
    val dtrace :
      before:('a -> unit) -> f:('a -> 'b) -> after:('b -> unit) -> 'a -> 'b
    val pwarn : string -> unit
    val wprintf : ('a, unit, string, unit) format4 -> 'a
  end
module type DEBUG_MOD_INFO =
sig
  val name : string
  val default : [ `Debug | `NoDebug ]
end
module NoDebug : DEBUG
module Make : functor (Module : DEBUG_MOD_INFO) -> DEBUG
