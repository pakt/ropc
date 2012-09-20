open BatListFull

let init_ro = ref false
let inputs = ref []
and streaminputs = ref None
and pintrace = ref false

(* Set garbage collector options whenever we see -trace. *)
let set_gc () =
  Gc.set
    {
      (Gc.get ()) with
	Gc.minor_heap_size = 32000000; (* 128 mb *)
	Gc.major_heap_increment = 16000000; (* 64 mb *)
	Gc.max_overhead = 100; (* compact after 100% overhead *)
    }

let stream_speclist =
  (* let addinput i = streaminputs := i :: !streaminputs in *)
  [
    ("-tracestream",
     Arg.String(fun s -> streaminputs := Some(`Tracestream s)),
     "<file> Read a trace to be processed as a stream.");

    ("-pin",
     Arg.Set pintrace,
     "Enable pin trace.");
  ]

let speclist =
  let addinput i = inputs := i :: !inputs in
  let toint64 s =
    try Int64.of_string s
    with Failure "int_of_string" -> raise(Arg.Bad("invalid int64: "^s))
  in
  let setint64 r s =  r := toint64 s in
  [
    ("-init-ro", Arg.Set (init_ro), "Access rodata.");
    ("-bin",
     Arg.String(fun s-> addinput (`Bin s)),
     "<file> Convert a binary to the IL");
    ("-binrange",
     Arg.Tuple(let f = ref ""
               and s = ref 0L in
               [Arg.Set_string f; Arg.String(setint64 s);
                Arg.String(fun e->addinput(`Binrange(!f, !s, toint64 e)))]),
     "<file> <start> <end> Convert the given range of a binary to the IL");
    ("-trace",
     Arg.String(fun s ->
		  set_gc () ;
		  addinput (`Trace s)),
     "<file> Read in a trace and lift it to the IL");
    ("-il",
     Arg.String(fun s -> addinput (`Il s)),
     "<file> Read input from an IL file.");
    ("-ir", (* to be removed in next versions *)
     Arg.String(fun s -> addinput (`Il s)),
     "<file> Read input from an IL file. (deprecated)");
    ("-pin", (* enable pin trace *)
     Arg.Set pintrace,
     "Enable pin trace");
	("-always-vex", Arg.Set Asmir.always_vex, "Only use vex to lift to IL" );
  ]



let get_program () =
  if !inputs = [] then raise(Arg.Bad "No input specified");
  let get_one (oldp,oldscope) = function
    | `Il f ->
      let newp, newscope = Parser.program_from_file ~scope:oldscope f in
	List.append newp oldp, newscope
    | `Bin f ->
	let p = Asmir.open_program f in
	List.append (Asmir.asmprogram_to_bap ~init_ro:!init_ro p) oldp, oldscope
    | `Binrange (f, s, e) ->
	let p = Asmir.open_program f in
	List.append (Asmir.asmprogram_to_bap_range ~init_ro:!init_ro p s e) oldp, oldscope
    | `Trace f ->
      List.append (Asmir.bap_from_trace_file ~pin:!pintrace f) oldp, oldscope
  in
 (* let rec cat p = function *)
  (*   | [] -> p *)
  (*   | arg::args -> cat (List.rev_append (List.rev (get_one arg)) p) args *)
  (* in *)
  try
    List.fold_left get_one ([], Grammar_scope.default_scope ()) (List.rev !inputs)
  with _ -> failwith "An exception occured while lifting"

let get_stream_program () = match !streaminputs with
  | None -> raise(Arg.Bad "No input specified")
  | Some(`Tracestream f) -> Asmir.bap_stream_from_trace_file ~pin:!pintrace f


(*  with fixme -> raise(Arg.Bad "Could not open input file")*)
