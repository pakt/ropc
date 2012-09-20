(**
   Module for executing SMTs inside of BAP

   XXX: This is not portable in any shape or way. It will only work on
   Unix-like systems.

   @author ejs
*)

exception Alarm_signal_internal;;
exception Alarm_signal of int;;

module D = Debug.Make(struct let name = "Smtexec" and default=`NoDebug end)
open D

open Unix

type model = (string*int64) list option

type result = Valid | Invalid (*of model*) | SmtError | Timeout

(** Class type to embed SMT execution functions.  If OCaml < 3.12 had
    first-class modules, we wouldn't need this. *)
class type smtexec =
object
  method printer : Formulap.fppf
  method solvername : string
  method solve_formula_file : ?printmodel:bool -> ?timeout:int -> string -> result
  (* XXX: Add other methods *)
end

module type SOLVER_INFO =
sig
  val timeout : int (** Default timeout in seconds *)
  val solvername : string (** Solver name *)
  val cmdstr : string -> string (** Given a filename, produce a command string to invoke solver *)
  val parse_result : ?printmodel:bool -> string -> string -> Unix.process_status -> result (** Given output, decide the result *)
  val printer : Formulap.fppf
end

(* Output type *)
module type SOLVER =
sig
  val solve_formula_file : ?printmodel:bool -> ?timeout:int -> string -> result (** Solve a formula in a file *)
  val check_exp_validity : ?timeout:int -> ?exists:(Ast.var list) -> ?foralls:(Ast.var list) -> Ast.exp -> result (** Check validity of an exp *)
  val create_cfg_formula :
    ?exists:Ast.var list ->  ?foralls:Ast.var list -> ?remove:bool
    -> Cfg.AST.G.t -> string
  val si : smtexec
end

let select_timeout = 0.1 (* seconds *)

let alarm_handler i =
  raise Alarm_signal_internal

let compute_wp_boring cfg post =
  let gcl = Gcl.of_astcfg cfg in
    (Wp.wp gcl post, [])

(* let query_formula ?timeout ?exists ?foralls f = *)
(*   let filename = write_formula ?exists ?foralls f in *)
(*   runstp ?timeout filename *)

(** Convert space delimited command string (command arg1 arg2 ... ) to a command and argument array *)
let split_cmdstr cmdstr =
  let slist = BatString.nsplit cmdstr " " in
  let cmd = List.hd slist in
  let args = Array.of_list slist in
  cmd, args

let syscall cmd =
  let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle alarm_handler) in
  let (stdoutread,stdoutwrite) = pipe () in
  let (stderrread,stderrwrite) = pipe () in
  let (stdinread,stdinwrite) = pipe () in
  let cmdname, args = split_cmdstr cmd in
  let pid = create_process cmdname args stdinread stdoutwrite stderrwrite in
  close stdoutwrite;
  close stderrwrite;
  close stdinread;
  close stdinwrite;
  (try
     let obuf = Buffer.create 16 in
     let ebuf = Buffer.create 16 in
     let fdlist = [stdoutread; stderrread] in
     List.iter set_nonblock fdlist;
     let wait = ref true in
     let estatus = ref None in
     let buf = String.create 1 in
     while !wait do

       (* Read any new data from stdout/stderr *)
       let rd timeout  =
	 let rl,_,_ = select fdlist [] fdlist timeout in
	 List.iter
	   (fun fd ->
	      let buffer = if fd=stdoutread then obuf else ebuf in
	      try
		while true do
		  (match read fd buf 0 1 with
		   | 1 ->
		       Buffer.add_string buffer buf
		   | 0 -> raise Exit
		   | _ ->
		       failwith "Assertion error");
		done
	      with Exit | Unix_error(EWOULDBLOCK,_,_) | Unix_error(EAGAIN,_,_) -> ()
	   ) rl;
(* 	 dprintf "Hmm: %d %d" (List.length rl) (List.length el); *)
       in

       (* Do a read *)
       rd select_timeout;

       (* Check if the process is dead yet *)
       let pid',estatus' = waitpid [WNOHANG] pid in
       if pid' = pid then begin
	 wait := false;
	 estatus := Some(estatus')
       end;

       (* Do another read, in case the process died *)
       rd 0.0;

     done;
     close stdoutread;
     close stderrread;
     (Buffer.contents obuf), (Buffer.contents ebuf), Util.option_unwrap !estatus
   with Alarm_signal_internal ->
     close stdoutread;
     close stderrread;
     raise (Alarm_signal pid) )

let result_to_string result =
  match result with
  | Valid -> "Valid"
  | Invalid -> "Invalid"
  | SmtError -> "SmtError"
  | Timeout -> "Timeout"

module Make = functor (S: SOLVER_INFO) ->
struct
  (** Write given formula out to random filename and return the filename *)
    let write_formula ?(exists=[]) ?(foralls=[]) ?(remove=true) f  =
      let name, oc = Filename.open_temp_file ("formula" ^ (string_of_int (getpid ())))  ".stp" in
      at_exit (fun () ->
	         if remove then
		   try
		     Unix.unlink name
		   with _ -> ());
      dprintf "Using temporary file %s" name;
      let pp = S.printer oc in
      pp#valid_ast_exp ~exists:exists ~foralls:foralls f;
      pp#flush;
      (*     output_string oc "QUERY(FALSE); COUNTEREXAMPLE;\n"; *)
      pp#close;
      name

    (** Write formula for AST CFG out to random filename and return the filename *)
    let create_cfg_formula ?(exists=[]) ?(foralls=[]) ?remove p  =
      let p = Prune_unreachable.prune_unreachable_ast p in
      let post = Ast.exp_true in
      let (wp, _foralls) = compute_wp_boring p post in
      let m2a = new Memory2array.memory2array_visitor () in
      let wp = Ast_visitor.exp_accept m2a wp in
      let foralls = List.map (Ast_visitor.rvar_accept m2a) foralls in
      (* FIXME: same for exists? *)
      write_formula ~exists ~foralls ?remove wp

    let solve_formula_file ?(printmodel=false) ?(timeout=S.timeout) file =
      ignore(alarm timeout);
      let cmdline = S.cmdstr file in

      try
        dprintf "Executing: %s" cmdline;
	let sout,serr,pstatus = syscall cmdline in

	(* Turn the alarm off *)
	ignore(alarm 0);

        dprintf "Parsing result...";
	S.parse_result ~printmodel sout serr pstatus

      with Alarm_signal(pid) ->
	kill pid 9;
	Timeout

    let check_exp_validity ?timeout ?exists ?foralls f =
      let filename = write_formula ?exists ?foralls f in
      solve_formula_file ?timeout filename

    class c = object(self)
      method solvername = S.solvername
      method solve_formula_file = solve_formula_file
      method printer = S.printer
    end

    let si = new c

  end;;

let parse_model solver s =
  dprintf "solver: %s stdout: %s" solver s;
  try
    let lexbuf = Lexing.from_string s in
    let solution = match solver with
      | "stp" -> Stp_grammar.main Stp_lexer.token lexbuf
      | "cvc3" -> Cvc3_grammar.main Cvc3_lexer.token lexbuf
      | "yices" -> Yices_grammar.main Yices_lexer.token lexbuf
      | _ -> failwith "Unknown solver"
    in
    Lexing.flush_input lexbuf;
    solution
  with Parsing.Parse_error ->
    None

let print_model = function
  | Some(l) -> Printf.printf "Model:\n"; List.iter (fun (v,i) -> Printf.printf "%s -> %#Lx\n" v i) l
  | None -> Printf.printf "No model found\n"

module STP_INFO =
struct
  let timeout = 60
  let solvername = "stp"
  let cmdstr f = "stp -p " ^ f
  let parse_result_builder solvername ?(printmodel=false) stdout stderr pstatus =
    let failstat = match pstatus with
      | WEXITED(c) -> c > 0
      | _ -> true
    in
    let fail = failstat || BatString.exists stderr "Fatal" in
    let isinvalid = BatString.exists stdout "Invalid." in
    let isvalid = BatString.exists stdout "Valid." in

    (*       dprintf "fail: %b %b %b" fail isinvalid isvalid; *)

    if isvalid then
      Valid
    else if isinvalid then (
      if printmodel then
        (let m = parse_model solvername stdout in
        print_model m);
      Invalid
    ) else if fail then (
      dprintf "output: %s\nerror: %s" stdout stderr;
      SmtError
    )
    else
      failwith "Something weird happened."
  let parse_result = parse_result_builder solvername
  let printer = ((new Stp.pp_oc) :> Formulap.fppf)
end

module STP = Make(STP_INFO)

module STPSMTLIB_INFO =
struct
  let timeout = 60
  let solvername = "stp"
  let cmdstr f = "stp --SMTLIB1 -t " ^ f
  let parse_result_builder solvername ?(printmodel=false) stdout stderr pstatus =
    let failstat = match pstatus with
      | WEXITED(c) -> c > 0
      | _ -> true
    in
    let fail = failstat || BatString.exists stderr "Fatal" in
    let issat = BatString.exists stdout "sat" in
    let isunsat = BatString.exists stdout "unsat" in

    (*       dprintf "fail: %b %b %b" fail isinvalid isvalid; *)

    if isunsat then
      Valid
    else if issat then (
      if printmodel then
        (let m = parse_model solvername stdout in
         print_model m);
      Invalid
    ) else if fail then (
      dprintf "output: %s\nerror: %s" stdout stderr;
      SmtError
    )
    else
      failwith "Something weird happened."
  let parse_result = parse_result_builder solvername
  let printer = ((new Smtlib1.pp_oc) :> Formulap.fppf)
end

module STPSMTLIB = Make(STPSMTLIB_INFO)

module CVC3_INFO =
struct
  let timeout = 60
  let solvername = "cvc3"
  let cmdstr f = "cvc3 +model " ^ f
  (* let parse_result stdout stderr pstatus = *)
  (*   let failstat = match pstatus with *)
  (*     | WEXITED(c) -> c > 0 *)
  (*     | _ -> true *)
  (*   in *)
  (*   let fail = failstat || BatString.exists stderr "Fatal" in *)
  (*   let isinvalid = BatString.exists stdout "Invalid." in *)
  (*   let isvalid = BatString.exists stdout "Valid." in *)

  (*   (\*       dprintf "fail: %b %b %b" fail isinvalid isvalid; *\) *)

  (*   if isvalid then *)
  (*     Valid *)
  (*   else if isinvalid then *)
  (*     Invalid *)
  (*   else if fail then ( *)
  (*     dprintf "CVC output: %s\nCVC error: %s" stdout stderr;   *)
  (*     SmtError *)
  (*   ) *)
  (*   else *)
  (*     failwith "Something weird happened." *)
  (* let parse_result stdout stderr pstatus = *)
  (*   let failstat = match pstatus with *)
  (*     | WEXITED(c) -> c > 0 *)
  (*     | _ -> true *)
  (*   in *)
  (*   let fail = failstat || BatString.exists stderr "Fatal" in *)
  (*   let isinvalid = BatString.exists stdout "Invalid." in *)
  (*   let isvalid = BatString.exists stdout "Valid." in *)

  (*   (\*       dprintf "fail: %b %b %b" fail isinvalid isvalid; *\) *)

  (*   if isvalid then *)
  (*     Valid *)
  (*   else if isinvalid then ( *)
  (*     let m = parse_model solvername stdout in *)
  (*     print_model m; *)
  (*     Invalid *)
  (*   ) else if fail then ( *)
  (*     dprintf "output: %s\nerror: %s" stdout stderr; *)
  (*     SmtError *)
  (*   ) *)
  (*   else *)
  (*     failwith "Something weird happened." *)
  let parse_result = STP_INFO.parse_result_builder solvername
  let printer = ((new Stp.pp_oc) :> Formulap.fppf)
end

module CVC3 = Make(CVC3_INFO)

module CVC3SMTLIB_INFO =
struct
  let timeout = 60
  let solvername = "cvc3"
  let cmdstr f = "cvc3 -lang smtlib " ^ f
  let parse_result = STPSMTLIB_INFO.parse_result_builder solvername
  let printer = ((new Smtlib1.pp_oc) :> Formulap.fppf)
end

module CVC3SMTLIB = Make(CVC3SMTLIB_INFO)

module YICES_INFO =
struct
  let timeout = 60
  let solvername = "yices"
  let cmdstr f = "yices -m " ^ f
  let parse_result = STPSMTLIB_INFO.parse_result_builder solvername
  let printer = ((new Smtlib1.pp_oc) :> Formulap.fppf)
end

module YICES = Make(YICES_INFO)

let solvers = Hashtbl.create 10;;
List.iter (fun (n,s) -> Hashtbl.add solvers n s)
  (
    ("stp", STP.si)
    ::("stp_smtlib", STPSMTLIB.si)
    ::("cvc3", CVC3.si)
    ::("cvc3_smtlib", CVC3SMTLIB.si)
    ::("yices", YICES.si)
    ::[]
  )

let runstp = STP.solve_formula_file
let query_formula = STP.check_exp_validity
