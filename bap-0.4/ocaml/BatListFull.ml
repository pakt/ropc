(** Opening this module will put a more tail-recursive List module
    into scope. *)

(** A complete list module that uses Batteries functions when
    possible.

    Why is this not in batteries itself?
*)

module List = struct
  include List
  include BatList
end
