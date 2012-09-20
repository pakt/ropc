let usage = "Usage: "^Sys.argv.(0)^" <input options> [transformations and outputs]\n\
             Transform BAP IL programs. "


open Utils_common

type ast = Ast.program
type astcfg = Cfg.AST.G.t
type ssa = Cfg.SSA.G.t

type prog =
  | Ast of ast
  | AstCfg of astcfg
  | Ssa of ssa

type cmd = 
  | TransformAst of (ast -> ast)
  | TransformAstCfg of (astcfg -> astcfg)
  | TransformSsa of (ssa -> ssa)
  | ToCfg
  | ToAst
  | ToSsa
 (* add more *)

let pipeline = ref []
let startdebug = ref 1


let output_ast f p =
  let oc = open_out f in
  let pp = new Pp.pp_oc oc in
  pp#ast_program p;
  pp#close;
  p

let output_ast_bbids f p =
  let oc = open_out f in
  Cfg_pp.AstBBidDot.output_graph oc p;
  close_out oc;
  p

let output_ast_cdg f p =
  let oc = open_out f in 
  let cdg = Depgraphs.CDG_AST.compute_cdg p in 
    Cfg_pp.AstBBidDot.output_graph oc cdg;
    close_out oc;
    p
 
let output_ast_pdg f p = 
  let oc = open_out f in 
  let pdg = Depgraphs.PDG_AST.compute_pdg p in 
    Cfg_pp.AstStmtsDot.output_graph oc pdg;
    close_out oc;
    p 

let output_ssa f p =
  let oc = open_out f in
  Cfg_pp.SsaStmtsDot.output_graph oc p;
  close_out oc;
  p
let output_ssa_bbids f p =
  let oc = open_out f in
  Cfg_pp.SsaBBidDot.output_graph oc p;
  close_out oc;
  p

let output_ssa_cdg f p =
  let oc = open_out f in 
  let cdg = Depgraphs.CDG_SSA.compute_cdg p in 
    Cfg_pp.SsaBBidDot.output_graph oc cdg;
    close_out oc;
    p

let output_ssa_ddg f p = 
  let oc = open_out f in 
  let ddg = Depgraphs.DDG_SSA.compute_ddg p in 
    Cfg_pp.SsaStmtsDot.output_graph oc ddg;
    close_out oc;
    p

let output_c f p =
  let oc = open_out f in
  let ft = Format.formatter_of_out_channel oc in
  let pp = new To_c.pp ft in 
  pp#ast_program p;
  close_out oc;
  p

let to_dsa p =
  let p,_ = Traces.to_dsa p in
  p 

let output_structanal p =
  let cfg = Prune_unreachable.prune_unreachable_ast p in
  let _ = Structural_analysis.structural_analysis cfg in
  (* FIXME: print a pretty graph or something. For now the debugging
     output is useful enough... *)
  p

let sccvn p =
  fst(Sccvn.replacer p)
let deadcode p =
  fst(Deadcode.do_dce p)
let jumpelim p =
  fst(Ssa_simp_misc.cfg_jumpelim p)
let ast_coalesce = Coalesce.AST_Coalesce.coalesce
let ssa_coalesce = Coalesce.SSA_Coalesce.coalesce
(* let memory2scalardef p = *)
(*   Memory2scalar.convert_g p Memory2scalar.Default *)
(* let memory2scalariroptir p = *)
(*   Memory2scalar.convert_g p Memory2scalar.IndirectROPTIR *)

(* Chop code added *)
let ast_chop srcbb srcn trgbb trgn p = 
  Ast_slice.CHOP_AST.chop p !srcbb !srcn !trgbb !trgn
let ssa_chop srcbb srcn trgbb trgn p = 
  Ssa_slice.CHOP_SSA.chop p !srcbb !srcn !trgbb !trgn

let add c =
  pipeline := c :: !pipeline

let uadd c =
  Arg.Unit(fun()-> add c)

let speclist =
  ("-pp-ast", Arg.String(fun f -> add(TransformAst(output_ast f))),
   "<file> Pretty print AST to <file>.")
  ::("-pp-ast-bbids", Arg.String(fun f -> add(TransformAstCfg(output_ast_bbids f))),
     "<file> Pretty print AST graph to <file> (in Graphviz format) (no stmts)")
  ::("-pp-ast-cdg", Arg.String (fun f -> add(TransformAstCfg(output_ast_cdg f))),
     "Output the AST CDG (bbid's)")
  ::("-pp-ast-pdg", Arg.String (fun f -> add(TransformAstCfg(output_ast_pdg f))),
     "Output the AST DDG (bbid's)")
  ::("-pp-ssa", Arg.String(fun f -> add(TransformSsa(output_ssa f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format)")
  ::("-pp-ssa-bbids", Arg.String(fun f -> add(TransformSsa(output_ssa_bbids f))),
     "<file> Pretty print SSA graph to <file> (in Graphviz format) (no stmts)")
  ::("-pp-ssa-cdg", Arg.String (fun f -> add(TransformSsa(output_ssa_cdg f))),
     "Output the SSA CDG (bbid's)")
  ::("-pp-ssa-ddg", Arg.String (fun f -> add(TransformSsa(output_ssa_ddg f))),
     "Output the SSA DDG (bbid's)")
  ::("-pp-novarnums", Arg.Unit (fun () -> Pp.output_varnums := false),
     "Print variables without variable ID numbers")
  ::("-struct", Arg.Unit (fun () -> add(TransformAstCfg(output_structanal))),
     "Structural analysis.")
  ::("-to-cfg", uadd(ToCfg),
     "Convert to an AST CFG.")
  ::("-to-ast", uadd(ToAst),
     "Convert to the AST.")
  ::("-to-ssa", uadd(ToSsa),
     "Convert to SSA.")
  :: ("-to-c", Arg.String(fun f -> add(TransformAst(output_c f))), 
      "<file> Output C to file."
     )
  ::("-ast-chop", 
      Arg.Tuple 
        (let srcbb = ref 0 and srcn = ref 0 
         and trgbb = ref 0 and trgn = ref 0 in
         [Arg.Set_int srcbb ; Arg.Set_int srcn ;
          Arg.Set_int trgbb ; Arg.Set_int trgn ; 
               uadd(TransformAstCfg(ast_chop srcbb srcn trgbb trgn)) ]),
     "<src-bb> <src-num> <trg-bb> <trg-num> Calculate the chop of an AST")
  ::("-ssa-chop", 
      Arg.Tuple 
        (let srcbb = ref 0 and srcn = ref 0 
         and trgbb = ref 0 and trgn = ref 0 in
         [Arg.Set_int srcbb ; Arg.Set_int srcn ;
          Arg.Set_int trgbb ; Arg.Set_int trgn ; 
               uadd(TransformSsa(ssa_chop srcbb srcn trgbb trgn)) ]),
     "<src-bb> <src-num> <trg-bb> <trg-num> Calculate the chop of an AST")
  ::("-sccvn", uadd(TransformSsa sccvn),
     "Apply Strongly Connected Component based Value Numbering")
  ::("-deadcode", uadd(TransformSsa deadcode),
     "Perform dead code ellimination.")
  ::("-ast-coalesce", uadd(TransformAstCfg ast_coalesce),
     "Perform coalescing on the AST.")
  ::("-ssa-coalesce", uadd(TransformSsa ssa_coalesce),
     "Perform coalescing on the SSA.")
  ::("-jumpelim", uadd(TransformSsa jumpelim),
     "Control flow optimization.")
  (* ::("-memtoscalar", uadd(TransformSsa memory2scalardef), *)
  (*    "Convert memory accesses to scalars (default mode).") *)
  (* ::("-memtoscalar-initro", uadd(TransformSsa memory2scalariroptir), *)
  (*    "Convert memory accesses to scalars (IndirectROPTIR mode).") *)
  ::("-ssa-simp", uadd(TransformSsa Ssa_simp.simp_cfg),
     "Perform all supported optimizations on SSA")
  ::("-ssa-to-single-stmt",
     uadd(TransformSsa Depgraphs.DDG_SSA.stmtlist_to_single_stmt),
     "Create new graph where every node has at most 1 SSA statement"
    )
  ::("-trace-cut", Arg.Int(fun i -> add(TransformAst(Util.take i))),
     "<n>  Get the first <n> instructions of the trace")
  ::("-trace-concrete", 
     uadd(TransformAst Traces.concrete),
     "Execute the trace concretely and obtain a straightline trace"
    )
  ::("-trace-concrete-subst", 
     uadd(TransformAst Traces.concrete_substitution),
     "Execute the trace concretely and obtain a straightline trace"
    )
  ::("-trace-slice", 
     uadd(TransformAst Traces.check_slice),
     "Slice a trace based on the overwritten return address"
    )
  ::("-trace-clean", 
     uadd(TransformAst Traces.clean),
     "Remove labels and comments from a concretized trace"
    )
  ::("-trace-reconcrete",
     Arg.String(fun f -> add(TransformAst(Traces.concrete_rerun f))),
     "Execute a concretized trace with the specified input file."
    )
  ::("-trace-dce",
     uadd(TransformAst Traces.trace_dce),
     "Trace dead-code elimination."
    )
  ::("-trace-start-debug",
     Arg.Set_int(startdebug),
     "Start debugging at item n."
    )
  ::("-trace-debug", 
     uadd(TransformAst Traces.trace_valid_to_invalid),
     "Formula debugging. Prints to files form_val and form_inv"
    )
  ::("-trace-conc-debug", 
     Arg.Unit 
       (fun () ->
	  let f = Traces.formula_valid_to_invalid ~min:!startdebug in
	  add(TransformAst f)
       ),
     "Formula debugging. Prints to files form_val and form_inv. Concretizes BEFORE debugging; useful for finding which assertion doesn't work."
    )
  ::("-trace-dsa",
     uadd(TransformAst to_dsa),
     "Convert to DSA form.")
   ::("-trace-target", 
     Arg.String (fun i -> add(TransformAst(Traces.control_flow i))),
     "<addr> Provide the target address <addr>"
    )   
   ::("-trace-symbolic-target", 
     uadd(TransformAst Traces.limited_control),
     "Use a symbolic jump target (to determine the amount of control we have)"
    )   
   ::("-trace-payload", 
     Arg.String (fun p -> add(TransformAst(Traces.add_payload p))),
     "<binstring> Provide a payload to be inserted at the return address (BEWARE of null bytes)"
    )   
   ::("-trace-payload-file", 
     Arg.String (fun p -> add(TransformAst(Traces.add_payload_from_file p))),
     "<binfile> Provide a payload to be inserted at the return address"
    )
   ::("-trace-payload-after-file", 
     Arg.String (fun p -> add(TransformAst(Traces.add_payload_from_file_after ~offset:4L p))),
     "<binfile> Provide a payload to be inserted past the return address"
    )
   ::("-trace-payload-after", 
     Arg.String (fun p -> add(TransformAst(Traces.add_payload_after ~offset:4L p))),
     "<binstring> Provide a payload to be inserted past the return address (BEWARE of null bytes)"
    )   
   ::("-trace-shell", 
     Arg.Int (fun off -> add(TransformAst(Traces.inject_shellcode off))),
     "<nopsled> Insert shellcode with a nopsled of the given size"
    )   
  ::("-trace-pivot",
     Arg.Tuple(
       let gaddr = ref 0L in
       let maddr = ref 0L in
       [
   	 Arg.String (fun a -> gaddr := Int64.of_string a);
  	 Arg.String (fun a -> maddr := Int64.of_string a);
  	 Arg.String (fun a -> add(TransformAst(Traces.add_pivot !gaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <payload string> Use pivot at gaddress to transfer control to payload at maddress."
    )
  ::("-trace-pivot-file",
     Arg.Tuple(
       let gaddr = ref 0L in
       let maddr = ref 0L in
       [
   	 Arg.String (fun a -> gaddr := Int64.of_string a);
  	 Arg.String (fun a -> maddr := Int64.of_string a);
  	 Arg.String (fun a -> add(TransformAst(Traces.add_pivot_file !gaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <payload string> Use pivot at gaddress to transfer control to payload at maddress."
    )
  ::("-trace-seh-pivot",
     Arg.Tuple(
       let gaddr = ref 0L in
       let maddr = ref 0L in
       let sehaddr = ref 0L in
       [
  	 Arg.String (fun a -> gaddr := Int64.of_string a);
  	 Arg.String (fun a -> maddr := Int64.of_string a);
	 Arg.String (fun a -> sehaddr := Int64.of_string a);
  	 Arg.String (fun a -> add(TransformAst(Traces.add_seh_pivot !gaddr !sehaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <sehaddress> <payload string> Use pivot at gaddress to transfer control (by overwriting SEH handler at sehaddress) to payload at maddress."
    )
  ::("-trace-seh-pivot-file",
     Arg.Tuple(
       let gaddr = ref 0L in
       let maddr = ref 0L in
       let sehaddr = ref 0L in
       [
  	 Arg.String (fun a -> gaddr := Int64.of_string a);
  	 Arg.String (fun a -> maddr := Int64.of_string a);
	 Arg.String (fun a -> sehaddr := Int64.of_string a);
  	 Arg.String (fun a -> add(TransformAst(Traces.add_seh_pivot_file !gaddr !sehaddr !maddr a)));
       ]),
     "<gaddress> <maddress> <sehaddress> <payload file> Use pivot at gaddress to transfer control (by overwriting SEH handler at sehaddress) to payload at maddress."
    )
  ::("-trace-formula", 
     Arg.String(fun f -> add(TransformAst(Traces.output_formula f))),
     "<file> Output the STP trace formula to <file>"
    )
  ::("-trace-formula-format",
     Arg.Set_string Traces.printer,
     "Set formula format (STP (default) or smtlib1)."
  )
  ::("-trace-exploit", 
     Arg.String(fun f -> add(TransformAst(Traces.output_exploit f))),
     "<file> Output the exploit string to <file>"
    )
  ::("-trace-assignments", 
     uadd(TransformAst(Traces.add_assignments)),
     "Explicitly assign the concrete values to the trace variables"
    )
  ::("-trace-length", 
     uadd(TransformAst(Traces.trace_length)),
     "Output the length of the trace"
    )
  ::("-trace-no-padding", 
     Arg.Unit(fun () -> Traces.padding := false),
     "Apply padding for symbolic unused bytes."
    )   
  ::("-no-let-bindings", 
     Arg.Clear Traces.full_symbolic,
     "Disable the usage of let bindings during formula generation"
    )
  ::("-trace-symbolic-indices", 
     Arg.Set Traces.allow_symbolic_indices,
     "Allow the existence of symbolic indices during formula generation"
    )
  ::("-trace-check",
     Arg.Set Traces.consistency_check,
     "Perform extra consistency checks"
    )
  ::("-trace-check-all",
     Arg.Unit(fun () -> Traces.consistency_check := true;
	   Traces.checkall := true),
     "Perform extra consistency checks"
    )
  ::("-trace-noopt",
     Arg.Clear Traces.dce,
     "Disable trace optimizations"
    )
  :: ("-normalize-mem", uadd(TransformAst Memory2array.coerce_prog),
      "Normalize memory accesses as array accesses")
  :: ("-prune-cfg",
      uadd(TransformAstCfg Prune_unreachable.prune_unreachable_ast),
      "Prune unreachable nodes from an AST CFG")
  :: ("-cfg-coalesce", uadd(TransformAstCfg Coalesce.AST_Coalesce.coalesce),
      "Perform coalescing on an AST-CFG graph")
  :: ("-unroll",
      Arg.Int (fun i -> add (TransformAstCfg(Unroll.unroll_loops ~count:i))),
      "<n> Unroll loops n times")
  :: ("-rm-backedges", uadd(TransformAstCfg Hacks.remove_backedges),
      "Remove backedges")
  :: ("-typecheck", uadd(TransformAst typecheck),
      "Typecheck program")
  :: Input.speclist

let anon x = raise(Arg.Bad("Unexpected argument: '"^x^"'"))
let () = Arg.parse speclist anon usage

let pipeline = List.rev !pipeline

let prog =
  try fst (Input.get_program())
  with Arg.Bad s ->
    Arg.usage speclist (s^"\n"^usage);
    exit 1

let rec apply_cmd prog = function
  | TransformAst f -> (
      match prog with
      | Ast p -> Ast(f p)
      | _ -> failwith "need explicit translation to AST"
    )
  | TransformAstCfg f -> (
      match prog with
      | AstCfg p -> AstCfg(f p)
      | _ -> failwith "need explicit translation to AST CFG"
    )
  | TransformSsa f -> (
      match prog with
      | Ssa p -> Ssa(f p)
      | _ -> failwith "need explicit translation to SSA"
    )
  | ToCfg -> (
      match prog with
      | Ast p -> AstCfg(Cfg_ast.of_prog p)
      | Ssa p -> AstCfg(Cfg_ssa.to_astcfg p)
      | AstCfg _ as p -> prerr_endline "Warning: null transformation"; p
    )
  | ToAst -> (
      match prog with
      | AstCfg p -> Ast(Cfg_ast.to_prog p)
      | p -> apply_cmd (apply_cmd p ToCfg) ToAst
    )
  | ToSsa -> (
      match prog with
      | AstCfg p -> Ssa(Cfg_ssa.of_astcfg p)
      | p -> apply_cmd (apply_cmd p ToCfg) ToSsa
    )
;;

List.fold_left apply_cmd (Ast prog) pipeline


