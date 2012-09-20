(** Constants for asmir.ml.  These could go in asmir.ml, but then we'd
   need an entry in asmir.mli for each one, and well, I'm just lazy.
*)

(* bfd defs *)

let (<<) = (lsl)

let bsf_debugging = 1 << 2
let bsf_function = 1 << 3

(** no flags *)
let bsec_no_flags = 0x000
(** allocate space when loading *)
let bsec_alloc = 0x001
(** load the section during loading *)
let bsec_load = 0x002
(** section has reloc info *)
let bsec_reloc = 0x004
(** read only *)
let bsec_readonly = 0x008
(** code *)
let bsec_code = 0x010
(** data *)
let bsec_data = 0x020
(** ROM *)
let bsec_rom = 0x040
(* others .... *)

