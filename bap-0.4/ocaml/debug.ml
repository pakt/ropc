(** Debugging module.

    This module contains a bunch of functions for printing debugging output
    an'at.

    Environment Variables:
    - [BAP_LOGFILE]
    If set, debug messages will be written to the file 
    $BAP_LOGFILE. The default is to write to standard error.
    - [BAP_DEBUG_MODULES]
    Specifies which modules should have debugging enabled or disabled. The list
    is separated by colons ([:]). {i modulename} or [!]{i modulename} turn
    debugging on or off respectively for {i modulename}. If {i modulename} is
    the empty string, it matches all modules. The leftmost match wins. Default
    behavior is undefined and subject to change. Note that {i modulename}
    refers to the name used for debug printing, which is not necesarily
    identical to the module name in OCaml.

    - [BAP_WARN_MODULES]
    Specifies which modules should have warning enabled or
    disabled. The format is like that of [BAP_DEBUG_MODULES].
    Default behavior is on (unlike debugging).

    - [BAP_DEBUG_TIMESTAMPS]
    Specified how (and whether) timestapms will be printed with each
    debug message. Supported values are [unix], [iso], [elapsed], and [none].
*)

(** prints given string exactly to the debug channel *)
let debug_string = prerr_string

(** like [debug_string] but prints a newline and flushes the buffer *)
let debug_endline = prerr_endline

let output_endline oc s =
  output_string oc s;
  output_char oc '\n';
  flush oc

(* Use the appropriate logfile, as per environment variables *)
let (debug_string,debug_endline) =
  try
    let filename = Sys.getenv "BAP_LOGFILE" in
    let oc = open_out_gen [Open_append;Open_creat] 0o664 filename in
    (output_string oc, output_endline oc)
  with Not_found -> (debug_string,debug_endline)


let get_env_options varname defvalue = 
  let default _ = defvalue in 
    try
      let modules = Sys.getenv varname in
      let len = String.length modules in
      let lookup name =
	let rec f spos =
	  let npos =
	    try String.index_from modules spos ':'
	    with Not_found -> len
	  in
	  if npos = spos then true else
	    let (sub, res) =
	      if String.get modules spos = '!'
	      then (String.sub modules (spos+1) (npos-spos-1), false)
	      else (String.sub modules spos (npos-spos), true)
	    in
	    if sub = "" || sub = name then res
	    else if npos < len then f (npos+1) else defvalue
	in
	f 0
      in
      lookup
    with Not_found ->
      default


(** [has_debug s] returns true when debugging is enabled for s.
    See documentation on [BAP_DEBUG_MODULES] at the top.
*)
let has_debug =
  get_env_options "BAP_DEBUG_MODULES"

(** [has_warn s] returns true when warnings are enabled for s.
    See documention on [BAP_WARN_MODULES] at the top *)
let has_warn = 
  get_env_options "BAP_WARN_MODULES" true


let indent = ref 0
let inc_indent () = indent := !indent + 1
let dec_indent () = indent := !indent - 1
let pindent () =
  let tab_is = 4 in (* number of "normal" indents *)
  let rec helper = function
    | 0 -> ()
    | n when n >= tab_is -> (debug_string "\t"; helper (n-tab_is))
    | n -> (debug_string "  "; helper (n-1))
  in
  helper (!indent)



let ptime_unix() =
  debug_string(Printf.sprintf "[%.3f]" (Unix.gettimeofday()))

let ptime_iso() =
  let secs = Unix.gettimeofday() in
  let t = Unix.localtime secs in
  Printf.ksprintf debug_string "[%4d-%2d-%2d_%2d:%2d:%2d]"
    t.Unix.tm_year t.Unix.tm_mon t.Unix.tm_mday
    t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec

let ptime_none() = ()

let ptime =
  try
    match Sys.getenv "BAP_DEBUG_TIMESTAMPS" with
    | "" | "unix" | "Unix" -> ptime_unix
    | "iso" | "ISO" -> ptime_iso
    | "none" | "None" -> ptime_none
    | "elapsed" ->
	let t0 = Unix.gettimeofday () in
	(fun () ->
	   Printf.ksprintf debug_string "[%f]" ((Unix.gettimeofday ()) -. t0) )
    | s ->
	prerr_endline("Warning: Unknown BAP_DEBUG_TIMESTAMPS value ("^s^")");
	ptime_unix
  with Not_found ->
    ptime_none
      
(** The type of module that contains the debugging functions *)
module type DEBUG =
sig
  val debug : bool
    (** Whether debugging is on *)

  val warn : bool
    (** Whether warnings are on *)

  val pdebug : string -> unit
    (** Prints given string as a debug message. *)

  val dprintf : ('a, unit, string, unit) format4 -> 'a
    (** printf a debug message *)

  val dtrace : before:('a->unit) -> f:('a->'b) -> after:('b->unit) -> 'a -> 'b
    (** [dtrace before f after] will return [f()], but will call
	[before] first and after calling [f] it will pass the result
	to [after]. Also, while [f] is running, indentation for all
	debugging functions will be increased.  *)
    (* more should get added here *)

  val pwarn : string -> unit
    (** Prints given string as a warning message. *)

  val wprintf : ('a, unit, string, unit) format4 -> 'a
    (** printf a warning message *)

end

(** The type of module we need to make a [DEBUG] module *)
module type DEBUG_MOD_INFO =
sig
  val name : string
    (** The name to prefix all debug messages with *)

  val default : [ `Debug | `NoDebug ]
    (** Specifies whether debugging is enabled or disabled by default. *)
end
  
(** No-op debugging module. Throws everything away. *)
module NoDebug : DEBUG  =
struct
  let debug = false
  let pdebug = (fun _ -> ())
  let dprintf fmt = Printf.ksprintf ignore fmt
  let dtrace = (fun ~before ~f ~after -> f)
  let warn = false
  let pwarn  = (fun _ -> ())
  let wprintf fmt = Printf.ksprintf ignore fmt
end

(** Creates a debugging module for the given component. *)
module Make(Module:DEBUG_MOD_INFO)  : DEBUG =
struct

  let debug = has_debug (Module.default = `Debug) Module.name 

  let pdebug s =
    pindent(); ptime();
    debug_string Module.name; debug_string ": ";
    debug_endline s
  let pdebug = if debug then pdebug else NoDebug.pdebug

  let dprintf fmt = Printf.ksprintf pdebug fmt
  let dprintf = if debug then dprintf else NoDebug.dprintf

  let dtrace ~before ~f ~after x =
    before x;
    inc_indent ();
    let r = f x in
    dec_indent ();
    after r;
    r
 
  let dtrace = if debug then dtrace else NoDebug.dtrace

  let warn = has_warn Module.name

  let pwarn s = 
    pindent(); ptime();
    debug_string "WARNING (";
    debug_string Module.name; 
    debug_string "): ";
    debug_endline s
    
  let pwarn = if warn then pwarn else NoDebug.pwarn

  let wprintf fmt = Printf.ksprintf pwarn fmt
  let wprintf = if warn then wprintf else NoDebug.wprintf

end
