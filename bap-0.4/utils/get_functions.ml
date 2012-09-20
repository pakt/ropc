
let usage = "Usage: "^Sys.argv.(0)^" <elf file> (<output prefix> | -r) [<function names>]\n\
             Disassemble functions from a binary."

let rangeonly = ref false
let unroll = ref false
let speclist =
  ("-r", Arg.Set rangeonly,
   "Print ranges rather than disassembling functions.")
  ::("-unroll", Arg.Set unroll, "Unroll loops.")
  ::[]

let file = ref ""
let prefix = ref ""
let names = ref []
let n = ref 0
let anon x =
  (match !n with
   | 0 -> file := x
   | 1 -> prefix := x
   | _ -> names := x :: !names
  );
  incr n
;;

Arg.parse speclist anon usage;
names := List.rev !names;
if !rangeonly && !prefix <> "" then
  names := !prefix :: !names;
if !file = "" then (
  Arg.usage speclist usage;
  exit 1
);
if !prefix = "" then rangeonly := true

let p = Asmir.open_program !file
let ranges = Asmir.get_function_ranges p

let doit = match !rangeonly with
  | true ->
      (fun (n,s,e) -> Printf.printf "%s\t0x%Lx 0x%Lx\n" n s e)
  | false ->
      (fun (n,s,e) ->
         try
         let ir = Asmir.asmprogram_to_bap_range p s e in
         let ir = Hacks.ret_to_jmp ir in
         (*let ir = Hacks.assert_noof ir in *)
         let cfg = Cfg_ast.of_prog ir in
         let cfg = Prune_unreachable.prune_unreachable_ast cfg in
         let cfg = if !unroll then
             let cfg = Unroll.unroll_loops cfg in
             Hacks.remove_backedges cfg
           else cfg
         in
         let cfg = Prune_unreachable.prune_unreachable_ast cfg in
         (* let oc = open_out "unroll.out" in *)
         (* let ssa_func_cfg = Cfg_ssa.of_astcfg cfg in *)
         (* Cfg_pp.SsaStmtsDot.output_graph oc ssa_func_cfg; *)
         (* close_out oc; *)
         let ir = Cfg_ast.to_prog cfg in
         let oc = open_out (!prefix ^ n ^ ".il") in
         let pp = new Pp.pp_oc oc in
         pp#ast_program ir;
         pp#close;
         with
         | ex ->
           Printf.eprintf "Warning: problem with %s (0x%Lx-0x%Lx): %s\n" n s e (Printexc.to_string ex);
      )
;;
let filter_range (s,_,_) =
  List.mem s !names
let ranges = if List.length !names > 0 then List.filter filter_range ranges else ranges;;

List.iter doit ranges
