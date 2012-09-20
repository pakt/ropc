open Symbeval
open Printf
open Sys
open Int_utils

type candidate = Cand of (int * int) * Ast.stmt list

let g_start = ref 0
let g_end = ref 0

let mAX_BYTES_BACKWARD = 10
let hALT = Ast.Halt(Ast.exp_true, [])

(* scan for RETs *)
let scan_section get s s_start s_end = 
    let l = ref [] in
    let _ = 
    for i=s_start to s_end-1 do
        let j = Int64.of_int i in
        let b = get j in
        if b = char_of_int 0xC3 then
            l := i::!l
        else
            ()
    done
    in
    !l
    
let make_scan get = scan_section get

let get_range_prog prog i j = 
    let (i,j) = cast_range i j in
    let l = Asmir.asmprogram_to_bap_range prog i j in
    l

let make_get_range prog = get_range_prog prog 

(* l must be sorted increasingly *)
let convert prog positions s_start = 
    let get_range = make_get_range prog in
    let get_small_range prev_pos pos = 
        let f i = 
            let s = (pos-i) in
            let e = pos+1 in (* +1 for RET (0xc3) *)
            let stmts = get_range s e in
            Cand((s,e), stmts)
        in
        let n = min mAX_BYTES_BACKWARD (pos - prev_pos - 1) in
        let _ = if n<0 then failwith "seems like an unsorted positions list" else () in
        (* return list of ranges [[..], [..], ...] *)
        Util.mapn f n
    in
    let f (acc, prev_pos) pos = 
        let l = get_small_range prev_pos pos in
        (l::acc, pos)
    in
    let _ = Printf.printf "s_start = 0x%08x\n" s_start in
    let (ranges, _) = List.fold_left f ([], s_start) positions in
    List.concat ranges

let make_convert prog = convert prog

let dump_range range = 
    let _ = Printf.printf "---\n" in
    (*
    let dumper = Pp.ast_short_dump in
    let range = List.map (fun x -> let AsmMeta(_,_,_,l) = x in l) range in
    let range = List.concat range in 
    let strings = dumper range in
    let _ = List.map (fun s -> Printf.printf "%s\n" s) strings in
    let _ = Printf.printf "---\n" in
    *)
    ()

let dump_ranges ranges = 
    let _ = Printf.printf "# NEW SECTION\n" in
    let _ = List.map (fun l -> dump_range l) ranges in
    ()

let write_to_file fn s = 
    let oc = open_out fn in 
    let _ = fprintf oc "%s" s in
    let _ = close_out oc in
    ()

let filter_sections prog f = 
    let sections = Asmir.get_all_asections prog in
    let sections = Array.to_list sections in
    let sections = List.filter f sections in
    sections

let sec_start_end sections = 
    let ss s = Asmir.get_section_start s in
    let se s = Asmir.get_section_end s in
    let triples = List.map (fun s -> (s, safe_to_int64 (ss s), safe_to_int64 (se s))) sections in
    triples

let candidates_all_sections prog = 
    let sections = filter_sections prog Asmir.is_code in
    let get = Asmir.get_prog_contents prog in
    let triples = sec_start_end sections in
    (*let _ = test_get get in*)
    let scan = make_scan get in
    let f (s,ss,se) = 
        let positions = scan s ss se in
        let positions = List.sort compare positions in
        (s, positions, ss) 
    in
    let sec_positions = List.map f triples in
    let convert_to_bap = make_convert prog in
    let f (s, positions, s_start) = 
        let ranges = convert_to_bap positions s_start in
        (s, ranges)
    in
    let sec_ranges = List.map f sec_positions in
    (* 
    let _ = List.map (fun (s, ranges) -> dump_ranges ranges) sec_ranges in
    *)
    let ranges = List.map (fun (x,y) -> y) sec_ranges in
    let ranges = List.concat ranges in

    (* just test *)
    if !g_start > 0 && !g_end > !g_start then
        let s,e = (!g_start, !g_end) in
        let l = get_range_prog prog s e in
        let c = Cand((s,e),l) in
        let ranges = [c] in
        ranges
    else
        ranges 

(* delta - var,value hash *)
let dump_ctx_delta delta = 
    Concrete.print_values delta

(* Special() is a sign of disasm error, execution will fail on these *)
let good_disasm stmts = 
    let p x = 
        match x with
        | Ast.Halt(_,_)
        | Ast.Special(_,_) ->
            true
        | _ -> false
    in
    let trash = List.exists p stmts in
    not trash

(*  don't allow ANY jumps.
    incomplete, but sufficient, to avoid infinite loops *)
let no_jumps stmts = 
    let p x = 
        match x with
        | Ast.Jmp(_,_)
        | Ast.CJmp(_,_,_,_) ->
            true
        | _ -> false
    in
    let trash = List.exists p stmts in
    not trash

(* filter bad disasms and potential infinite loops *)
let filter_candidates candidates = 
    (* get stmts *)
    let gs cand = let Cand(_,stmts) = cand in stmts in
    let fm cand = let Cand(m,_) = cand in m in
    let candidates = List.filter (fun c -> good_disasm (gs c)) candidates in
    (* RET gets split into few instructions, last of them being jmp *)
    let candidates = List.map (fun c -> Cand(fm c, Common.drop_last (gs c))) candidates in
    let candidates = List.filter (fun c -> no_jumps (gs c)) candidates in
    candidates

module GadgetSet = Set.Make(
    struct
        let compare = Pervasives.compare
        type t = Common.gadget
    end
    )

let gadget_set_of_list = List.fold_left (fun acc x -> GadgetSet.add x acc) GadgetSet.empty

(* g_list is a list of (f_init,f_finish) functions of gadgets *)
let classify g_list cand = 
    let Cand((off_s,off_e), stmts) = cand in
    let org_stmts = stmts in
    let add_halt stmts = 
        stmts @ [hALT]
    in
    let stmts = add_halt stmts in
    let cinit = Gdefs.common_init stmts in

    let f acc g_func = 
        let empty_set = GadgetSet.empty in
        let (_, _, _, repeats) = g_func () in
        let co_exec (gset,_,_) i =
            (* g_func might want to initiate regs/mem to random values with each run,
               so this is necessary. computing the tuple just once would make the registers constant *)
            let (f_init, f_finish, mem_hax, _) = g_func () in
            let init = f_init stmts in (* FIXME: should return init, stmts? *)
            let init = cinit @ init in
            let ctx = Symbeval.concretely_execute ~i:init ~mem_hax:mem_hax (stmts) in
            let gadgets, mod_regs = f_finish ctx in
            (* let _ = Printf.printf "len g = %d\n" (List.length gadgets) in  *)
            let gset_new = gadget_set_of_list gadgets in
            let gset = if i=0 then gset_new else GadgetSet.inter gset gset_new in
            (* mod_regs are going to be verified later, so we don't have to be very precise *)
            gset, mod_regs, Some(ctx)
        in
        let range = Util.mapn (fun x->x) (repeats-1) in (* len(range) = repeats *)
        let (gset, mod_regs, ct) = List.fold_left co_exec (empty_set, [], None) range in
        let ctx =
            match ct with
            | Some(ctx) -> ctx
            | None -> assert false
        in
        let gadgets = GadgetSet.elements gset in
        (* we are not saving stmts, because marshaling them doesn't work for some reason *)
        (* verifier will not accept Halt/Special statements *)
        let wrapped = Gdefs.wrap_meta ctx gadgets mod_regs off_s off_e in 
        wrapped::acc
    in
    let gadgets = List.fold_left f [] g_list in
    let gadgets = List.concat gadgets in
    gadgets

let make_classify = classify (Gdefs.g_list ())

let biggest_rw_data prog = 
    (* let datas = filter_sections prog Asmir.is_rw_data in *)
    let datas = filter_sections prog Asmir.is_bss in 
    let sse = sec_start_end datas in
    let sse = List.map (fun (_,s,e) -> (e-s,s,e)) sse in
    let cmp (s1,_,_) (s2,_,_) = compare s1 s2 in
    let sorted = List.sort cmp sse in
    let sorted = List.map (fun (_,s,e) -> (s,e)) sorted in
    let data = List.nth sorted ((List.length sorted)-1) in
    data

let parse_exe fn = 
    let prog = Asmir.open_program fn in
    let candidates = candidates_all_sections prog in
    let classify = make_classify in
    let candidates = filter_candidates candidates in
    let gmetas = List.map (fun cand -> classify cand) candidates in
    let gmetas = List.concat gmetas in
    (*
    let _ = Printf.printf "len gmetas: %d\n" (List.length gmetas) in
    let pr gm = let Common.GMeta(g,_,_,_)=gm in Printf.printf "# %s\n" (Common.dump_gadget g) in
    let _ = List.map pr gmetas in
    *)
    let data = biggest_rw_data prog in
(*
    let (s,e) = data in
    let _ = Printf.printf "biggest: %x-%x\n" s e in
    let gmetas = [] in
*)
    gmetas, data

(*
let dump_gmetas gmetas = 
    let dumped = List.map (fun gm -> Gdefs.dump_gmeta gm) gmetas in
    let _ = List.map (fun s -> Printf.printf "---\n%s\n" s) dumped in
    ()
*)

let wrap_in_container fn gmetas data = 
    let cont = Common.GContainer(fn, data, gmetas) in
    cont

let set_start_end_offsets s e = 
    let id = fun x -> x in
    begin
    (g_start := Scanf.sscanf s "0x%x" id);
    (g_end := Scanf.sscanf e "0x%x" id);
    Printf.printf "start: %x\n" !g_start;
    Printf.printf "end: %x\n" !g_end;
    end

let main () =
    let parse_and_save fn ofn =
        let gmetas, data = parse_exe fn in
        (* let _ = dump_gmetas gmetas in *)
        let container = wrap_in_container fn gmetas data in
        Common.marshal_to_file ofn container
    in
    let argc = Array.length Sys.argv in
    if argc > 1 then
        let fn = Sys.argv.(1) in
        let ofn = if argc = 4 then
            let s = Sys.argv.(2) in
            let e = Sys.argv.(3) in
            let _ = set_start_end_offsets s e in
            "candidates.bin" 
        else 
            if argc = 3 then
                Sys.argv.(2)
            else
                assert false
        in
        parse_and_save fn ofn
    else 
        let err = Printf.sprintf "Usage:\n%s <exe file> <out candidates file>\n" Sys.argv.(0) in
        print_string err
(*
let main () = 
    try
        _main ()
    with _ ->
        let s = Printexc.get_backtrace () in
        Printf.printf "%s\n" s 
*)
let _ = main()
