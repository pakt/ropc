(* Assumes the parser file is parser.mly and the lexer file is "lexer.mll". *)

open Common
open Printf
open Ast (* for types *)
open Cdefs
open Analysis

let hARDCODED_PRINTF = 0x080484a0
let cRASH_ADDRESS = 0x1111
let sTACK_VAR_OFF = 0
let fRAME_VAR_OFF = 4
let dATA_OFF = 8 (* start writing tables here *)

let nO_NAME_LABEL = "@@"
let gLOBAL_END_LABEL = "global_end"

let trd (_,_,x) = x
let i32 = Int32.of_int
let fun_label id = "function_"^id
let fun_local_label fun_id id = "local_"^fun_id^"_"^id

let print_errors errors = 
    let errors = Ast.dump_errors errors in
    let rec aux errors =
        match errors with
        | hd::tl ->
            let hd = "ERROR. "^hd^"\n" in
            let _ = printf "%s" hd in
            aux tl 
        | [] -> ()
    in
    aux errors

let pick_different_reg used = 
    let regs = Common.rEGS_NO_ESP in
    let p reg = try let _ = List.find (fun x->x=reg) used in false with Not_found -> true in
    let not_used = List.filter p regs in
    match not_used with
    | hd::tl -> hd
    | _ -> assert false

(* every invocation of f_next_reg returns new symbolic reg *)
let make_rewrite_exp f_next_reg read_local ref_local =
    let rec rewrite_exp exp oreg = 
        match exp with
        | BinOp(exp1, op, exp2) ->
            let reg1 = f_next_reg () in
            let iexp1 = rewrite_exp exp1 reg1 in
            let reg2 = f_next_reg () in
            let iexp2 = rewrite_exp exp2 reg2 in
            let o = BinO(oreg, reg1, op, reg2) in
            iexp1 @ iexp2 @ [o]
        | UnOp(op, exp) -> 
            begin
            match op with
            | Sub -> rewrite_exp (BinOp(Const(0),op,exp)) oreg
            | Not -> rewrite_exp (BinOp(Const(-1),Xor,exp)) oreg
            | _ -> assert false
            end
        | Var(id) ->
            let rl = read_local id oreg in
            [rl]
        | Ref(id) ->
            let rl = ref_local id oreg in
            [rl]
        | ReadMem(id) ->
            let addr_reg = f_next_reg () in 
            let rl = read_local id addr_reg in
            let rm = ReadM(oreg, addr_reg) in
            [rl;rm]
        | Const(x) ->
            let mov = MovRegConst(oreg, x) in
            [mov]
    in
    rewrite_exp

let rewrite_stmt stack_ptr frame_ptr locals fun_id stmt = 
    let f_next_reg = make_reg_generator () in
    let rw_local id reg f_ctor = 
        let id2off id = try Hashtbl.find locals id with Not_found -> assert false in
        let off = id2off id in
        f_ctor off reg
    in
    let write_local id reg = 
        rw_local id reg (fun off reg -> WriteLocal(off,reg))
    in
    let read_local id reg = 
        rw_local id reg (fun off reg -> ReadLocal(off,reg))
    in
    let ref_local id reg = 
        rw_local id reg (fun off reg -> LocalAddr(off, reg))
    in
    let deref_local id reg = 
        let f off reg =
            let addr_reg = f_next_reg () in
            let rl = ReadLocal(off, addr_reg) in
            let wm = WriteM(addr_reg, reg) in
            [rl;wm]
        in
        rw_local id reg f
    in
    let rewrite_exp = make_rewrite_exp f_next_reg read_local ref_local in
    let push_arg arg = 
        (* just check if args are simple *)
        let _ = match arg with
            | Var(_) | ReadMem(_) | Ref(_) | Const(_) -> true
            | _ -> assert false 
        in
        let reg = f_next_reg () in
        let iarg = rewrite_exp arg reg in
        let push = PushReg(reg) in
        iarg @ [push]
    in
    let push_args args =
        let rec aux acc args = 
            match args with
            | arg::tl -> 
                let pa = push_arg arg in
                aux (pa::acc) tl 
            | [] -> acc
        in
        let pushes = aux [] args in
        (* let pushes = List.rev pushes in *)
        List.concat pushes
    in
    let set_eax_on_cond cond = 
        (* ah = SF ZF xx AF xx PF xx CF *)
        (* returns mask and the value required to take the jump *)
        let flag_mask_const flag = 
            let mask,v = 
                match flag with
                | E -> (1 lsl 6), 1 lsl 6   (* jz, ZF = 1 *)
                | A -> (1 lor (1 lsl 6)), 0 (* ja, CF = ZF = 0 *)
                | B -> 1, 1                 (* jb, CF = 1 *)
                | _ -> assert false
            in
            (* flags are saved to AH, so shift *)
            let mask,v = mask lsl 8, v lsl 8 in
            mask, v
        in
        let neg, flags = 
            match cond with
            | Cond(flags) -> false, flags
            | NCond(flags) -> true, flags
        in
        (* FIXME: just one flag atm *)
        let flag = match flags with hd::[] -> hd | _ -> assert false in
        if flag=MP then 
            let mov = MovRegConst(C(EAX), 1) in
            [mov]
        else
            let mask, v = flag_mask_const flag in
            let reg = f_next_reg () in
            let mov1 = MovRegConst(reg, mask) in
            let and_ah = BinO(C(EAX), C(EAX), And, reg) in
            let reg = f_next_reg () in
            let mov2 = MovRegConst(reg, v) in
            let sub = BinO(C(EAX), C(EAX), Sub, reg) in
            let lahf = SaveFlags in
            let reg = f_next_reg () in
            (* ZF position in EAX: 6th bit of AH *)
            let mov3 = MovRegConst(reg, 1 lsl (6+8)) in 
            let shr = BinO(C(EAX), C(EAX), Div, reg) in
            let reg = f_next_reg () in
            let mov4 = MovRegConst(reg, 1) in
            (* eax=1 iff cond *)
            let last = [BinO(C(EAX), C(EAX), And, reg)] in
            let last = last @ 
                if neg then [BinO(C(EAX), C(EAX), Xor, reg)]
                else []
            in
            [mov1;and_ah;mov2;sub;lahf;mov3;shr;mov4]@last
    in
    let rewrite stmt = 
        match stmt with
        | Assign(id, exp) ->
            let reg = f_next_reg () in
            let iexp = rewrite_exp exp reg in
            let wl = write_local id reg in
            iexp @ [wl]
        | DerefAssign(id, exp) ->
            let reg = f_next_reg () in
            let iexp = rewrite_exp exp reg in
            let wl = deref_local id reg in
            iexp @ wl
        | WriteMem(id, exp) ->
            let exp_reg = f_next_reg () in
            let iexp = rewrite_exp exp exp_reg in
            let addr_reg = f_next_reg () in
            let rl = read_local id addr_reg in
            let wm = WriteM(addr_reg, exp_reg) in
            iexp @ [rl;wm]
        | Cmp(exp1, exp2) ->
            let reg1, reg2 = f_next_reg (), f_next_reg () in
            let iexp1 = rewrite_exp exp1 reg1 in
            let iexp2 = rewrite_exp exp2 reg2 in
            let reg = f_next_reg () in
            let sub = BinO(reg, reg1, Sub, reg2) in 
            let lahf = SaveFlags in
            iexp1 @ iexp2 @ [sub; lahf]
        | Call(id, ExpArgs(exp_args)) ->
            let pushes = push_args exp_args in
            let reg = f_next_reg () in
            let mov = MovRegSymb(reg, FromTo(Named(fun_label id), Unnamed(Forward))) in
            let p = PushReg(reg) in
            let reg = f_next_reg () in
            let mov2 = MovRegSymb(reg, FromTo(Unnamed(Forward), Named(fun_label id))) in
            let add = OpStack(Add, reg) in (* jmp *)
            let lbl = Lbl(nO_NAME_LABEL) in
            pushes @ [mov;p;mov2;add;lbl]
        | ExtCall(id, ExpArgs(exp_args)) -> 
            let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
            let make_filler n = 
                let m x = let x = x land 0xFF in (x lsl 24) lor (x lsl 16) lor (x lsl 8) lor x in
                let f acc x = RawHex(m x)::acc in
                let nums = range 0 n in
                let filler = List.fold_left f [] nums in
                let filler = List.rev filler in
                filler
            in
            let store_args imp_addr args = 
                let addr_reg = f_next_reg () in
                let v_reg = f_next_reg () in
                let off_reg = f_next_reg () in
                let fix_reg = f_next_reg () in

                let per_arg acc arg = 
                    let iarg = rewrite_exp arg v_reg in
                    let wm = WriteM(addr_reg, v_reg) in
                    let set = MovRegConst(off_reg, 4) in
                    let add = BinO(addr_reg, addr_reg, Add, off_reg) in
                    acc @ (iarg@[wm;set;add]) (* O(n^2) *)
                in
                let tmp_reg = f_next_reg () in
                let lbl = Lbl(nO_NAME_LABEL) in
                let save_esp = MovRegReg(tmp_reg, C(ESP)) in
                let mov = MovRegSymb(fix_reg, FromTo(Unnamed(Backward), Unnamed(Forward))) in
                let fix1 = BinO(addr_reg, tmp_reg, Add, fix_reg) in
                (* Restore import address *)
                let reg = f_next_reg () in
                let set_imp = MovRegConst(reg, imp_addr) in
                let wm = WriteM(addr_reg, reg) in
                let reg = f_next_reg () in
                let set8 = MovRegConst(reg, 8) in
                let fix2 = BinO(addr_reg, addr_reg, Add, reg) in
                let stores = List.fold_left per_arg [] args in
                [save_esp;lbl;mov;fix1;set_imp;wm;set8;fix2] @ stores
            in
            let jmp_over_locals locals_filler = 
                let n = List.length locals_filler in
                let reg = f_next_reg () in
                let mov = MovRegConst(reg, n*4) in
                let ops = OpStack(Add, reg) in
                [mov;ops]
            in
            let imp_addr = hARDCODED_PRINTF in
            let cmt_s = sprintf "jmp %s" id in
            (* At least 128 bytes for locals *)
            let n_args = List.length exp_args in
            let locals_filler = make_filler (256/4) in
            let jmp_skip_locals = jmp_over_locals locals_filler in
            (* FIXME: hardcoded printf *)
            let jmp_imp = RawHex(imp_addr) in
            (* FIXME: we don't need equality in AdvStack, just >= *)
            let adv = AdvanceStack(n_args*4+4) in
            let lbl = Lbl(nO_NAME_LABEL) in
            let args_filler = make_filler n_args in
            let write_args = store_args imp_addr exp_args in
            write_args @ jmp_skip_locals @ locals_filler @ 
                [Comment(cmt_s);lbl;jmp_imp;adv] @ (args_filler)
        | Branch(cond, id) ->
            (* eax=1 iff cond, 0 otherwise *) 
            let setz = set_eax_on_cond cond in 
            let reg = f_next_reg () in
            let start = Unnamed(Forward) in
            let fin = Named(fun_local_label fun_id id) in
            let mov = MovRegSymb(reg, FromTo(start, fin)) in
            let mul = BinO(C(EAX), C(EAX), Mul, reg) in
            let add = OpStack(Add, C(EAX)) in (* jmp *)
            let lbl = Lbl(nO_NAME_LABEL) in
            setz @ [mov; mul; add; lbl;]
        | Label(id) -> [Lbl(fun_local_label fun_id id)]

        | Enter(n) ->
                let reg = f_next_reg () in
                let rm1 = ReadMConst(reg, frame_ptr) in
                let push = PushReg(reg) in
                let reg = f_next_reg () in
                let rm2 = ReadMConst(reg, stack_ptr) in
                let wm1 = WriteMConst(frame_ptr, reg) in
                let reg1 = f_next_reg () in
                let rm3 = ReadMConst(reg1, stack_ptr) in
                let reg2 = f_next_reg () in
                let mov = MovRegConst(reg2, n) in
                let reg3 = f_next_reg () in
                let sub = BinO(reg3, reg1, Sub, reg2) in
                let wm2 = WriteMConst(stack_ptr, reg3) in
                [rm1;push;rm2;wm1;rm3;mov;sub;wm2]
        | Leave -> 
                let reg = f_next_reg () in
                let rm = ReadMConst(reg, frame_ptr) in
                let wm1 = WriteMConst(stack_ptr, reg) in
                let reg = f_next_reg () in
                let pop = PopReg(reg) in
                let wm2 = WriteMConst(frame_ptr, reg) in
                [rm;wm1;pop;wm2]
        | Ret(id) -> 
                let reg1 = f_next_reg () in
                let reg2 = f_next_reg () in
                let reg3 = f_next_reg () in
                let p2 = PopReg(reg1) in
                let mov = MovRegSymb(reg2, FromTo(Unnamed(Forward), Named(fun_label id))) in
                let sub = BinO(reg3, reg2, Add, reg1) in
                let add = OpStack(Add, reg3) in (* jmp *)
                let lbl = Lbl(nO_NAME_LABEL) in
                [p2;mov;sub;add;lbl;]
        (* AssignTab is replaced with Assign(id,C) earlier *)
        | AssignTab(_,_) -> assert false
    in
    let new_instrs = rewrite stmt in
    let comments = 
        match stmt with
        | Label(_) -> []
        | _ -> 
            let s = Ast.dump_stmt stmt in 
            [Comment(s)]
    in
    comments @ new_instrs

let rewrite_prog prog stack_ptr frame_ptr = 
    let assign_vars func = 
        let collect_locals stmts = 
            let rec aux acc stmts =
                match stmts with
                (* all locals are initialized before use *)
                | (Assign(id,_))::tl -> aux (id::acc) tl
                | hd::tl -> aux acc tl
                | [] -> acc
            in
            let ids = aux [] stmts in
            let ids = Common.generic_unique ids in
            ids
        in
        let Fun(id, Args(args), FunBody(stmts)) = func in
        let h = Hashtbl.create 32 in 
        let f (h,n) arg = 
            let _ = Hashtbl.add h arg n in
            (h,n+4)
        in
        (* v1,frame,ret,arg1,...,argN *)
        let (h,_) = List.fold_left f (h,12) args in
        let g (h,n) id = 
            let _ = Hashtbl.add h id n in
            (h,n-4)
        in
        let ids = collect_locals stmts in
        let (h,_) = List.fold_left g (h,0) ids in
        h
    in
    let rewrite_func func = 
        let add_stack_stuff fun_id locals stmts = 
            let locals_count = Hashtbl.length locals in
            (* every local is a dword *)
            let pre = [Enter(locals_count*4)] in
            let suf = [Leave;Ret(fun_id)] in
            let stmts = pre@stmts@suf in
            stmts
        in
        let rewrite_stmt = rewrite_stmt stack_ptr frame_ptr in
        let Fun(fun_id, Args(args), FunBody(stmts)) = func in
        let locals = assign_vars func in 
        let stmts = add_stack_stuff fun_id locals stmts in
        let instrs = List.map (fun stmt -> rewrite_stmt locals fun_id stmt) stmts in
        let head = Ast.dump_func_head func in
        let fun_lbl = fun_label fun_id in
        let pre = [Comment(head); Lbl(fun_lbl);] in
        let instrs = [pre]@instrs in
        instrs
    in
    let Prog(func_list) = prog in
    let rew = List.map rewrite_func func_list in
    (* let rew = List.concat (rew) in *)
    rew

(* Extract tables and create a stub that writes them to the data section.
 * All AssignTable(id,list) are changed to Assign(id,C), where C is the 
 * address in .data section *)
let handle_tables data_s prog = 
    let per_func data_start func = 
        let h = Hashtbl.create 8 in
        let per_stmt acc stmt = 
            let (off, pairs, rew) = acc in
            match stmt with
            | AssignTab(id, l) -> 
                let new_stmt = Assign(id, Const(off)) in
                let new_off = off + List.length l in
                new_off,(off,l)::pairs,new_stmt::rew

            | _ -> off,pairs,stmt::rew
        in
        let Fun(fun_id, Args(args), FunBody(stmts)) = func in
        let data_end, pairs, stmts = List.fold_left per_stmt (data_start,[],[]) stmts in
        let stmts = List.rev stmts in
        let func = Fun(fun_id, Args(args), FunBody(stmts)) in
        data_end, pairs, func
    in
    let per_func_fold (data_start,l_pairs,funs) func = 
        let data_end, f_pairs, new_func = per_func data_start func in
        (data_end, f_pairs::l_pairs, new_func::funs)
    in
    let dump_pairs pairs = 
        let pr (off, l) = 
            let s = dump_int_list l in
            printf "0x%08x,%s\n" off s 
        in
        List.map pr pairs 
    in
    let make_stub pairs = 
        let store addr v = 
            let r = S(-1) in
            let mov = MovRegConst(r, v) in
            let wm = WriteMConst(addr, r) in
            [mov;wm]
        in
        let chop l n = 
            let f (i,a,b) x = if i<n then (i+1,x::a,b) else (i+1,a,x::b) in
            let (_,a,b) = List.fold_left f (0,[],[]) l in
            List.rev a, List.rev b
        in
        let to_int l = 
            let f acc x = (acc lsl 8)+x in
            List.fold_left f 0 l
        in
        let make_one off l = 
            let rec aux acc off l =
                let pre,suf = chop l 3 in
                match pre with
                | hd::tl -> 
                    let v = to_int (List.rev pre) in
                    let s = store off v in
                    aux (s::acc) (off+3) suf
                | [] -> List.flatten (List.rev acc)
            in
            let s = aux [] off l in
            s
        in
        let f acc (off,l) =
            let s = make_one off l in
            s::acc
        in
        let ss = List.fold_left f [] pairs in
        List.flatten (List.rev ss)
    in
    let data_start = data_s+dATA_OFF in
    let Prog(func_list) = prog in
    let (_,l_pairs,funs) = List.fold_left per_func_fold (data_start,[],[]) func_list in
    let funs = List.rev funs in
    let pairs = List.rev (List.flatten l_pairs) in
    let _ = dump_pairs pairs in
    let new_prog = Prog(funs) in
    let stub = make_stub pairs in
    stub, new_prog

let add_comments f_comment new_instrs prefix instr = 
    let comments = 
        if f_comment instr then
            let s = dump_instr instr in
            [Comment(prefix^s)]
        else
            []
    in
    comments @ new_instrs

(* concretize symbolic constants *)
(* IN: (instr,gm) pairs 
 * OUT: (instr,gm) pairs without MovRegSymb *)
let fix_symblic pairs = 
    let get_size gm = 
        let GMeta(_,_,_,stack_fix) = gm in
        stack_fix
    in
    let check_lbl label instr = match instr with Lbl(lab) -> label=lab | _ -> false in
    let distance_to_generic f_match pairs = 
        let rec aux dist pairs = 
            match pairs with
            | (instr,gm)::tl ->
                if f_match instr then 
                    (*
                    let _ = Printf.printf "found label %s in: %s\n" label
                    (dump_instr instr) in
                    *)
                    Some(dist)
                else 
                    begin
                    (* Ignore gmetas for labels and comments -_-' *)
                    if is_lbl_or_comment instr then
                        aux dist tl
                    else
                        let size = get_size gm in
                        aux (size+dist) tl
                    end
            | [] -> None
        in
        let dist = aux 0 pairs in
        dist
    in
    let distance_to_lbl lbl pairs = 
        let f_match = check_lbl lbl in
        let dist = distance_to_generic f_match pairs in
        dist
    in
    let try_both_ways id pre suf =
        let before = distance_to_lbl id pre in
        let after = distance_to_lbl id suf in
        match before,after with
        | Some(_),Some(_) -> failwith ("Found duplicate: "^id)
        | None, None -> failwith ("Can't find label:"^id)
        | Some(n),None -> -n
        | None,Some(n) -> n
    in
    let distance_to_unnamed dir pre suf = 
        let sign,chunk = 
            match dir with
            | Forward -> 1,suf
            | Backward -> -1,pre
        in
        let dist = distance_to_lbl nO_NAME_LABEL chunk in
        match dist with
        | Some(n) -> sign*n
        | None -> failwith "Unnamed not found"
    in
    let get_distance symb pre suf = 
        match symb with
        | Named(id) -> try_both_ways id pre suf 
        | Unnamed(dir) -> distance_to_unnamed dir pre suf 
    in
    let rec aux pre suf = 
        match suf with
        | (MovRegSymb(reg, FromTo(start, fin)),gm)::tl-> 
            let dstart = get_distance start pre suf in
            let dfin = get_distance fin pre suf in
            let dist = dfin-dstart in
            let _ = printf "FromTo: (%s,%s)->(%d,%d)->%d\n" (dump_symb start) (dump_symb fin) dstart dfin dist in
            let fix = MovRegConst(reg, dist) in
            aux ((fix,gm)::pre) tl
        | hd::tl -> aux (hd::pre) tl
        | [] -> List.rev pre
    in
    aux [] pairs

(* AdvanceStack -> RawHex.
 * to_binary would try to fill the gap before the return address,
 * but we use that space for arguments. *)
let fix_ext_call_stuff pairs =
    let get_addr gm = 
        let GMeta(_, fm, _, _) = gm in
        let FileMeta(off_s, _) = fm in
        off_s
    in
    let set_stack_fix gm sf = 
        let GMeta(g,fm,mod_reg,_) = gm in
        GMeta(g,fm,mod_reg,sf)
    in
    let f acc (instr,gmeta) = 
        let new_instr =
            match instr with
            | AdvanceStack(n) -> RawHex(get_addr gmeta)
            | _ -> instr
        in
        if new_instr <> instr then
            let cmt = Comment(dump_instr instr) in
            let fake_gm = set_stack_fix gmeta 4 in
            let p1 = (new_instr, fake_gm) in
            let p2 = (cmt, gmeta) in
            p1::p2::acc
        else (instr,gmeta)::acc
    in
    let pairs = List.fold_left f [] pairs in
    List.rev pairs

let write_const_const src_reg addr_reg addr value = 
    let m1 = MovRegConst(src_reg, value) in
    let m2 = MovRegConst(addr_reg, addr) in
    let wm1 = WriteM(addr_reg, src_reg) in
    [m1; m2; wm1]

let global_prefix_suffix data_s data_e = 
    let stack_top = data_e in
    let stack_frame = stack_top in
    let st_ptr = data_s+sTACK_VAR_OFF in (* global var holding stack_top *)
    let sf_ptr = data_s+fRAME_VAR_OFF in (* -- stack_frame *)
    let addr_reg, src_reg = S(-1), S(-2) in (* HACK *)
    let write_st = write_const_const src_reg addr_reg st_ptr stack_top in
    let write_sf = write_const_const src_reg addr_reg sf_ptr stack_frame in
    let reg = S(-3) in
    let mov = MovRegSymb(reg, FromTo(Named(fun_label "main"), Named(gLOBAL_END_LABEL))) in
    let push = PushReg(reg) in
    let lbl = Lbl(gLOBAL_END_LABEL) in
    let pre = write_st @ write_sf @ [mov; push] in
    let suf = [lbl] in
    pre, suf, st_ptr, sf_ptr

let to_binary_one io (instr,gm) = 
    let get_lc_off g = 
        match g with
        | LoadConst(_,off) -> off
        | _ -> assert false
    in
    let fill io n = 
        let dwords = n / 4 in
        let bytes = n mod 4 in
        let rec aux i f m = 
            if i < m then
                let _ = f n io in aux (i+1) f m
            else ()
        in
        let f_d n io = IO.write_i32 io n in
        let f_b n io = IO.write_byte io n in
        let _ = aux 0 f_d dwords in
        let _ = aux dwords f_b (dwords+bytes) in
        ()            
    in
    let value_to_write instr off_s = 
        match instr with
        | RawHex(v) -> v
        | _ -> off_s
    in
    let GMeta(g, fm, _, stack_fix) = gm in
    let FileMeta(off_s, _) = fm in
    let v = value_to_write instr off_s in
    let _ = IO.write_i32 io v in

    let _ = 
        match instr with
        | MovRegConst(r,v) -> 
                let off = get_lc_off g in
                let _ = assert (stack_fix - off - 4 >= 0) in
                let _ = fill io off in
                let _ = IO.write_i32 io v in
                fill io (stack_fix - off - 8) 
        | RawHex(_) -> assert (stack_fix = 4) 
        | _ -> fill io (stack_fix-4) 
    in
    (* return string *)
    ()

let filter_trash pairs = 
    let p (i,_) = 
        match i with
        | Lbl(_) | Comment(_) -> false
        | _ -> true
    in
    List.filter p pairs

let to_binary pairs = 
    let io = IO.output_string () in
    let consume acc (instr,gm) = 
        to_binary_one io (instr,gm)
    in
    let _ = List.fold_left consume () pairs in
    let _ = List.map (fun i -> IO.write_i32 io cRASH_ADDRESS) [1;2;3;4;5;6;7] in
    IO.close_out io

let dump_possible gadgets stack_ptr frame_ptr instrs = 
    let implement = make_implement stack_ptr frame_ptr in
    let p_by_arg, p_by_pos = make_possible_regs_funs gadgets implement in
    let f _ instr = 
        let _ = Printf.printf "%s - " (dump_instr instr) in
        let args = arg_dumper instr in
        let per_arg _ arg = 
            let _ = Printf.printf "| %s: " (dump_sreg arg) in
            let set = p_by_arg instr arg in
            let regs = RegSet.elements set in
            let _ = Common.generic_dumper (fun r -> Common.dump_reg r) regs in
            ()
        in
        let _ = List.fold_left per_arg () args in
        Printf.printf "%s" "\n"
    in
    let _ = List.fold_left f () instrs in
    ()

(* dump 'compiled' program *)
let dump_instrs cl = 
    let print i = 
        let s = dump_instr i in
        Printf.printf "%s\n" s in
    let _ = List.map print cl in
    ()

let dump_pairs pairs = 
    let _ = print_endline "~~~~~~~~~~~~~" in
    let pr acc (instr,gmeta) = 
        let GMeta(_,_,_,stack_fix) = gmeta in
        let (off, sep) = 
            if is_lbl_or_comment instr then
                acc, " "
            else
                acc+stack_fix, "\t"
        in
        let _ = printf "0x%04x%s%s\n" acc sep (dump_instr instr) in
        off
    in
    (* First RET will add 4 *)
    let _ = List.fold_left pr 4 pairs in
    ()

(* FIXME: main has to be at the beginning *)
let compile prog container = 
    let process_func assign_regs instr_lll = 
        let per_stmt acc instrs = 
            (* list of instructions, set of regs to preserve *)
            let impl = 
                try 
                    assign_regs instrs SRegSet.empty 
                with Not_found ->
                    let _ = dump_instrs instrs in
                    assert false
                in
            impl::acc
        in
        let per_func acc stmts = 
            let impl = List.fold_left per_stmt [] stmts in
            let impl = List.rev impl in
            impl::acc
        in
        let impl_lll = List.fold_left per_func [] instr_lll in
        List.rev impl_lll
    in
    let verify_impl impl = 
        let p instr = instr_type instr = T0 in
        let ok = List.for_all p impl in
        if not ok then assert false
        else ()
    in
    let GContainer(fn, (data_s, data_e), gmetas) = container in
    let gadgets = get_gadgets gmetas in
    let prefix, suffix, stack_ptr, frame_ptr = global_prefix_suffix data_s data_e in
    (* Swap AssignTable with Assign (const).
     * stub stores all tables in .data section *)
    let stub, prog = handle_tables data_s prog in
    (* Function to implement instructions in terms of simpler instructions.
     * Ultimately instruction is converted to a list of gadgets. *)
    let implement = make_implement stack_ptr frame_ptr in
    let assign_regs = make_assign_regs gmetas stack_ptr frame_ptr in
    (* instr list list list.
     * 1st level: list of functions 
     * 2nd level: list of (rewritten) stmts
     * 3rd level: instructions *)
    let instrs_ll = rewrite_prog prog stack_ptr frame_ptr in
    let instrs_ll = [stub] :: [prefix] :: instrs_ll @ [[suffix]] in
    let instrs_lll = [[[Comment("lol");Lbl("1")]]] in
    let instrs_lll = [[stub]] in
    let impl_lll = process_func assign_regs instrs_ll in
    let impl_ll = List.flatten impl_lll in
    let pairs = List.flatten impl_ll in
    let pairs = fix_ext_call_stuff pairs in

    let instrs = List.map fst pairs in
    let _ = dump_pairs pairs in
    let _ = verify_impl instrs in

    let pairs = fix_symblic pairs in
    let pairs = filter_trash pairs in
    let bin_str = to_binary pairs in
    instrs, pairs, bin_str

let parse_src src_fn = 
    let cin = open_in src_fn in
    let lexbuf = Lexing.from_channel cin in
    let p = Parser.input Lexer.token lexbuf in
    let errors = Ast.verify_prog p in
    (p, errors)

let main () =
    let argc = Array.length Sys.argv in
    if argc > 2 then
        let src_fn = Sys.argv.(1) in
        let vg_fn = Sys.argv.(2) in
        let out_fn = "compiled.bin" in
        let (p, errors) = parse_src src_fn in
        if errors <> [] then
            print_errors errors 
        else
            let p = Ast.unwrap_prog p in
            let p = Ast.move_main_to_front p in
            let p = Ast.flatten_prog p in
            let container = Common.unmarshal_from_file vg_fn in
            let s = Ast.dump_prog p in
            let cl, pairs, bin_str = compile p container in
            let _ = printf "DUMPED:\n%s\n####\n" s in
            let _ = write_str_to_file out_fn bin_str in
            ()
    else 
        let err = Printf.sprintf "Usage:\n%s <src fn> <vg fn>\n" Sys.argv.(0) in
        print_string err

let _ = main()
