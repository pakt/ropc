open Common
open Ast
open Cdefs
open Printf

let instr_type = function 
    (* T0, because we want them at every stage *)
    | Lbl(_) 
    | Comment(_)

    | AdvanceStack(_)
    | RawHex(_)
    | MovRegConst(_,_)
    | MovRegReg(_,_) 
    | MovRegSymb(_,_) 
    | WriteM(_,_) 
    | ReadM(_,_) 
    | SaveFlags
    | OpStack(_,_) 
    | BinO(_,_,_,_) -> T0

    | ReadMConst(_,_)
    | WriteMConst(_,_) -> T1

    | LocalAddr(_,_) 
    | PushReg(_) 
    | PopReg(_) -> T2

    | ReadLocal(_,_)
    | WriteLocal(_,_) -> T3

(* IN: instr, gmeta list *)
(* OUT: gmeta corresponding to instr *)
let find_all_gmetas instr gms = 
    let find_gms f_match = 
        let pred gm = 
            let GMeta(g,_,_,_) = gm in
            f_match g gm
        in
        List.filter pred gms 
    in
    let is_opstack g = match g with Common.OpEsp(_,_,_) -> true | _ -> false in
    let get_stack_fix gm = let GMeta(_,_,_,sf) = gm in sf in
    let f_match_movreg r g gm = match g with Common.LoadConst(gr,_) -> gr=r | _ -> false in
    let f_match_op_esp op r g gm = match g with Common.OpEsp(gop, gr, _) -> (gop=op && gr=r) | _ -> false in
    let f_match = 
        match instr with
        | OpStack(op, C(r)) ->
                let op' = ast_op_to_gadget_op op in
                f_match_op_esp op' r
        | BinO(C(r0), C(r1), op, C(r2)) -> 
                let op' = ast_op_to_gadget_op op in
                fun g gm -> g = Common.BinOp(r0, r1, op', r2)
        | WriteM(C(addr_reg), C(src_reg)) ->
                fun g gm -> g = Common.WriteMem(addr_reg, Int32.zero, src_reg) 
        | ReadM(C(dst_reg), C(addr_reg)) ->
                fun g gm -> g = Common.ReadMem(dst_reg, addr_reg, Int32.zero) 
        (* movregsymb will be converted to mov reg const *)
        | MovRegConst(C(r),_) | MovRegSymb(C(r),_) -> 
                f_match_movreg r
        | MovRegReg(C(dst), C(src)) ->
                fun g gm -> g = Common.CopyReg(dst,src) 
        | SaveFlags ->
                fun g gm -> g = Common.Lahf
        | AdvanceStack(n) ->
                fun g gm -> (not (is_opstack g)) && (get_stack_fix gm = n)
        (* Can match anything, but this simplifies things *)
        | RawHex(_) ->
                fun g gm -> get_stack_fix gm = 4
        (* we don't want to lose these, so match with anything *)
        | Lbl(_) | Comment(_) -> fun g gm -> true
        | _ -> assert false
    in
    let matching_gms = find_gms f_match in
    if matching_gms = [] then raise Not_found 
    else matching_gms


let make_implement stack_ptr frame_ptr = 
    let implement_t1 f_next_reg instr = 
        match instr with
        | ReadMConst(r, addr) ->
            let reg = f_next_reg () in
            let mov = MovRegConst(reg, addr) in
            let rm = ReadM(r, reg) in
            [mov;rm]
        | WriteMConst(addr,r) -> 
            let addr_reg = f_next_reg () in
            let mov = MovRegConst(addr_reg, addr) in
            let wm = WriteM(addr_reg, r) in
            [mov;wm]
        | _ -> assert false
    in
    let implement_t2 f_next_reg instr =
        match instr with
        | PushReg(r) ->
                let addr_reg = f_next_reg () in
                let rm = ReadMConst(addr_reg, stack_ptr) in
                let wm1 = WriteM(addr_reg, r) in
                let reg1 = f_next_reg () in
                let rm2 = ReadMConst(reg1, stack_ptr) in 
                let reg2 = f_next_reg () in
                let mov = MovRegConst(reg2, 4) in
                let reg3 = f_next_reg () in
                let sub = BinO(reg3, reg1, Sub, reg2) in
                let wm2 = WriteMConst(stack_ptr, reg3) in
                [rm;wm1;rm2;mov;sub;wm2]
        | PopReg(r) ->
                let reg1 = f_next_reg () in
                let rm1 = ReadMConst(reg1, stack_ptr) in
                let reg2 = f_next_reg () in
                let mov = MovRegConst(reg2, 4) in
                let reg3 = f_next_reg () in
                let sub = BinO(reg3, reg1, Add, reg2) in
                let wm = WriteMConst(stack_ptr, reg3) in
                let rm2 = ReadM(r, reg3) in
                [rm1;mov;sub;wm;rm2;]
        | LocalAddr(off,r) ->
                let reg1 = f_next_reg () in
                let rm1 = ReadMConst(reg1, frame_ptr) in
                let reg2 = f_next_reg () in
                let mov = MovRegConst(reg2, off) in
                let add = BinO(r, reg1, Add, reg2) in
                [rm1;mov;add;]
        | _ -> assert false
    in
    let implement_t3 f_next_reg instr =
        match instr with
        | ReadLocal(off, r) -> 
                let addr_reg = f_next_reg () in
                let la = LocalAddr(off, addr_reg) in
                let rm = ReadM(r, addr_reg) in
                [la;rm]
        | WriteLocal(off, r) ->
                let addr_reg = f_next_reg () in
                let la = LocalAddr(off, addr_reg) in
                let wm = WriteM(addr_reg, r) in
                [la;wm]
        (* Caller should be aware these are special *)
        | Lbl(_) 
        | Comment(_) -> assert false
        | _ -> assert false
    in
    let implement instr = 
        let type2idx = function T3 -> 2 | T2 -> 1 | T1 -> 0 | T0 -> assert false in
        let init () =
            let f_next_reg = make_reg_generator () in
            let funs = [implement_t1;implement_t2;implement_t3] in
            let funs = List.map (fun f -> f f_next_reg) funs in
            funs
        in
        let funs = init () in
        let typ = instr_type instr in
        let idx = type2idx typ in
        let f_implement = List.nth funs idx in
        f_implement instr
    in
    implement

let arg_dumper instr = 
    match instr with 
    | AdvanceStack(_) -> []
    | RawHex(_) -> []
    | MovRegConst(a1,_) -> [a1]
    | MovRegReg(a1,a2) -> [a1;a2]
    | MovRegSymb(a1,_) -> [a1]
    | WriteM(a1,a2) -> [a1;a2]
    | ReadM(a1,a2) -> [a1;a2]
    | SaveFlags -> []
    | OpStack(_,a1) -> [a1]
    | BinO(a1,a2,_,a3) -> [a1;a2;a3]

    | ReadMConst(a1,_)
    | WriteMConst(_,a1) -> [a1]

    | LocalAddr(_,a1) 
    | PushReg(a1)
    | PopReg(a1) -> [a1]

    | ReadLocal(_,a1)
    | WriteLocal(_,a1) -> [a1]
    | Lbl(_)
    | Comment(_) -> []

let number_of_args instr = List.length (arg_dumper instr)

let arg_positions instr arg = 
    let args = arg_dumper instr in
    let enum (i,l) x = (i+1,(i,x)::l) in
    let _, args = List.fold_left enum (0,[]) args in
    let args = List.filter (fun (_,a) -> a=arg) args in
    let positions = List.map fst args in
    positions

(* get possible regs at position pos for instructions matching type of instr.
 * for example: BinOp(r0,_,+,_) 0 -> possible values for r0 *)
let possible_regs_t0 gms instr = 
    let f_binop op = 
        let f acc g = 
            match g with
            | Common.BinOp(r0, r1, op', r2) -> if op = op' then ([r0;r1;r2]::acc) else acc
            | _ -> acc
        in
        f
    in
    let f_op_esp op = 
        let f acc g = 
            match g with
            | Common.OpEsp(op', r, _) -> if op = op' then ([r]::acc) else acc
            | _ -> acc
        in
        f
    in
    let f_write_mem acc g =
        match g with
        | Common.WriteMem(r0, _, r1) -> ([r0;r1]::acc)
        | _ -> acc
    in
    let f_read_mem acc g =
        match g with
        | Common.ReadMem(r0, r1, _) -> ([r0;r1]::acc)
        | _ -> acc
    in
    let f_load_const acc g =
        match g with
        | Common.LoadConst(r,_) -> ([r]::acc)
        | _ -> acc
    in
    let f_copy_reg acc g =
        match g with
        | Common.CopyReg(r0,r1) -> ([r0;r1]::acc)
        | _ -> acc
    in
    (* [[a1;..];[b1..]] -> [a1;b1],[[..];[..]] *)
    let group_args regs = 
        let f (heads,tails) l =
            match l with
            | hd::tl -> (hd::heads, tl::tails)
            | [] -> assert false
        in
        let rec aux acc ll =
            match ll with
            | (hd::_)::tll -> (* at least one non-empty list *)
                let (heads,tails) = List.fold_left f ([],[]) ll in
                aux (heads::acc) tails
            | _ -> List.rev acc
        in
        aux [] regs
    in
    let make_sets groups = 
        let f acc l = 
            let set = Cdefs.set_from_list l in
            set::acc
        in
        let sets = List.fold_left f [] groups in
        List.rev sets
    in
    let f_collect = 
        match instr with
        | OpStack(op, _) ->
                let op' = ast_op_to_gadget_op op in
                f_op_esp op' 
        | WriteM(_, _) -> f_write_mem
        | BinO(_, _, op, _) ->
                let op' = ast_op_to_gadget_op op in
                 f_binop op'
        | ReadM(_, _) -> f_read_mem
        (* movregsymb will be converted to mov reg const *)
        | MovRegConst(_,_) | MovRegSymb(_,_) -> f_load_const 
        | MovRegReg(_, _) -> f_copy_reg 
        | _ -> assert false
    in
    (* regs is a list of lists *)
    let regs = List.fold_left f_collect [] gms in
    let groups = group_args regs in
    let sets = make_sets groups in
    sets

let matching_func_for_instr instr = 
    let f_match = 
        match instr with
        | OpStack(op, _) -> 
                ( fun instr -> match instr with OpStack(op'',_) -> op'' = op | _ -> false)
        | WriteM(_, _) -> 
                ( fun instr -> match instr with WriteM(_,_) -> true | _ -> false)
        | BinO(_, _, op, _) -> 
                ( fun instr -> match instr with  BinO(_,_,op'',_) -> op'' = op | _ -> false)
        | ReadM(_, _) -> 
                ( fun instr -> match instr with ReadM(_,_) -> true | _ -> false)
        (* movregsymb will be converted to mov reg const *)
        | MovRegConst(_,_) | MovRegSymb(_,_) -> 
                ( fun instr -> match instr with 
                MovRegConst(_,_) | MovRegSymb(_,_) -> true |_ -> false)
        | MovRegReg(_, _) -> 
                ( fun instr -> match instr with MovRegReg(_,_) -> true | _ -> false)

        | ReadMConst(_,_) -> 
                ( fun instr -> match instr with ReadMConst(_,_) -> true | _ -> false )
        | WriteMConst(_,_) -> 
                ( fun instr -> match instr with WriteMConst(_,_) -> true | _ -> false)
        | LocalAddr(_,_) -> 
                ( fun instr -> match instr with LocalAddr(_,_) -> true | _ -> false)
        | PushReg(_)-> 
                ( fun instr -> match instr with PushReg(_) -> true | _ -> false)
        | PopReg(_) -> 
                ( fun instr -> match instr with PopReg(_) -> true | _ -> false)

        | ReadLocal(_,_)-> 
                ( fun instr -> match instr with ReadLocal(_,_) -> true | _ -> false)
        | WriteLocal(_,_) -> 
                ( fun instr -> match instr with WriteLocal(_,_) -> true | _ -> false)
        | AdvanceStack(_)->
                ( fun instr -> match instr with AdvanceStack(_) -> true | _ -> false)
        | RawHex(_) ->
                ( fun instr -> match instr with RawHex(_) -> true | _ -> false)
        | Lbl(_)-> assert false
        | Comment(_) -> assert false
        | _ -> assert false
    in
    f_match

(* Make f_assign x an identity for concrete regs *)
let wrap_f_assign f = 
    let g r = 
        match r with 
        | C(_) -> (try f r with _ -> r)
        | S(_) -> (try f r with _ -> assert false)
    in
    g

let apply_assignment f_assign instr = 
    let f = wrap_f_assign f_assign in
    match instr with
    | MovRegConst(r,c) -> MovRegConst(f r,c)
    | MovRegSymb(r,sc) -> MovRegSymb(f r,sc)
    | ReadMConst(r,ma) -> ReadMConst(f r,ma)
    | WriteMConst(ma,r) -> WriteMConst(ma,f r)
    | ReadLocal(off,r) -> ReadLocal(off,f r)
    | WriteLocal(off,r) -> WriteLocal(off,f r)
    | LocalAddr(v,r) -> LocalAddr(v,f r)
    | PopReg(r) -> PopReg(f r) 

    | PushReg(r) -> PushReg(f r) 
    | OpStack(op, r) -> OpStack(op, f r) 

    | MovRegReg(r1,r2) -> MovRegReg(f r1,f r2) 
    | WriteM(r1,r2) -> WriteM(f r1,f r2) 
    | ReadM(r1,r2) -> ReadM(f r1,f r2) 
    | BinO(ro,r1,op,r2) -> BinO(f ro, f r1, op, f r2) 

    (*
    | AdvanceStack
    | RawHex
    | SaveFlags 
    | Lbl(_)
    | Comment(_)
    *)
    | _ -> instr

let make_cache_funs () = 
    let cache = ref (fun i p -> raise Not_found) in
    let cache_add instr reg_set_list = 
        let f_match = matching_func_for_instr instr in
        let new_cache i p = 
            if f_match i then
                List.nth reg_set_list p
            else
                !cache i p (* FIXME ? *)
        in
        begin
            cache := new_cache
        end
    in
    let cache_test instr pos = 
        try let _ = !cache instr pos in true with Not_found -> false
    in
    let cache_get instr pos = 
        try !cache instr pos with Not_found -> assert false
    in
    (cache_add,cache_test,cache_get)

(* All registers used in implementations are "local". 
 * Use noncolliding regs for params: S(-1), S(-2) ... *)
let make_fake_instr instr = 
    let args = arg_dumper instr in
    let f (n,f_assign) arg = 
        let f_new x = if x=arg then (S(-n)) else f_assign x in 
        (n+1, f_new)
    in
    let f_assert = (fun x->assert false) in
    let (_, f_assign) = List.fold_left f (1, f_assert) args in
    let fake_instr = apply_assignment f_assign instr in
    fake_instr

(* *by_pos/by_arg are mutually recursive.
 * by_pos returns possible regs for ith argument
 * by_arg returns possible regs for a specific arg
 * difference: if two args are equal then we need to intersect corresponding
 * sets *)
let make_possible_regs_funs gadgets implement = 
    (* let (cache_add,cache_test,cache_get) = make_cache_funs () in *)
    let rec possible_regs_by_pos gadgets implement instr pos = 
        let possible_regs_t0 = possible_regs_t0 gadgets in
        let cache_add instr reg_set_list = 
            ()
        in
        (* FIXME *)
        let cache_test instr pos = false in
        let cache_get instr pos = assert false in
        let higher_t instr pos = 
            let process_impl impl arg = 
                let collect reg_set instr = 
                    let regs = possible_regs_by_arg gadgets implement instr arg in
                    RegSet.inter reg_set regs
                in
                List.fold_left collect fULL_REG_SET impl
            in
            (* Beware: this works correctly only because higher types don't take multiple reg params *)
            let fake_instr = make_fake_instr instr in
            let args = arg_dumper fake_instr in
            let impl = implement fake_instr in
            let f acc arg = 
                let regs = process_impl impl arg in 
                regs::acc
            in
            let possible_for_all_args = List.fold_left f [] args in
            List.rev possible_for_all_args
        in
        if (cache_test instr pos) then
            cache_get instr pos
        else
            (* list of sets. i-th set contains possible regs for ith param *)
            let reg_set_list = 
                let typ = instr_type instr in
                match typ with
                | T0 -> 
                        (* get possible regs for all arguments *)
                        let reg_set_list = possible_regs_t0 instr in
                        reg_set_list
                | _ -> higher_t instr pos
            in
            let _ = cache_add instr reg_set_list in
            let total_args = number_of_args instr in
            if pos > (total_args-1) then assert false
            else List.nth reg_set_list pos
    and
    possible_regs_by_arg gadgets implement instr arg = 
        let positions = arg_positions instr arg in
        let collect reg_set pos = 
            let regs = possible_regs_by_pos gadgets implement instr pos in
            RegSet.inter reg_set regs
        in
        List.fold_left collect fULL_REG_SET positions
    in
    let by_arg = possible_regs_by_arg gadgets implement in
    let by_pos = possible_regs_by_pos gadgets implement in
    by_arg, by_pos

let mod_read_vars = function
    | AdvanceStack(_)
    | RawHex(_) -> [],[]
    | MovRegConst(r,_)
    | MovRegSymb(r,_)
    | ReadMConst(r,_)
    | ReadLocal(_,r)
    | LocalAddr(_,r)
    | PopReg(r) -> [r],[]

    | WriteLocal(_,r)
    | WriteMConst(_,r)
    | PushReg(r) 
    | OpStack(_, r) -> [],[r]

    | MovRegReg(r1,r2) -> [r1],[r2]
    | WriteM(r1,r2) -> [],[r1;r2]
    | ReadM(r1,r2) -> [r1],[r2]
    | BinO(ro,r1,op,r2) -> [ro],[r1;r2]

    | SaveFlags -> [C(EAX)],[]
    | Lbl(_)
    | Comment(_) -> [],[]

let mod_vars instr = fst (mod_read_vars instr)
let read_vars instr = snd (mod_read_vars instr)

(* Assumes SSA form. 
 * Overapproximated for non-SSA.
 * Store first write and last read *)
let analyse_reads_writes instrs = 
    let reads = Hashtbl.create 8 in
    let writes = Hashtbl.create 8 in
    let update_hashes i instr reads writes =
        let wr = mod_vars instr in
        let rd = read_vars instr in
        (* first write *)
        let f_w acc reg = 
            try
                let _ = Hashtbl.find writes reg in
                ()
            with Not_found -> 
                Hashtbl.add writes reg i
        in
        (* last read *)
        let f_r acc reg = Hashtbl.add reads reg i in 
        let _ = List.fold_left f_w () wr in
        let _ = List.fold_left f_r () rd in
        ()
    in
    let f i instr = 
        let _ = update_hashes i instr reads writes in
        i+1
    in
    let _ = List.fold_left f 0 instrs in
    (reads, writes)

let get_kv h = 
    (* Hashtbl.fold provides history of bindings, but we only want the most
     * recent one. *)
    let seen = Hashtbl.create 8 in
    let f k v acc = 
        try 
            let _ = Hashtbl.find seen k in
            acc
        with Not_found ->
            let _ = Hashtbl.add seen k true in
            (k,v)::acc
    in
    let l = Hashtbl.fold f h [] in
    l

let inverse_hash h = 
    let inv = Hashtbl.create 16 in
    let kv_pairs = get_kv h in
    let f h (k,v) = 
        let cur = try Hashtbl.find h v with Not_found -> SRegSet.empty in
        let cur = SRegSet.add k cur in
        let _ = Hashtbl.add h v cur in
        h
    in
    let inv = List.fold_left f inv kv_pairs in
    inv

let hash_get h k empty = try Hashtbl.find h k with Not_found -> empty

let find_read_but_not_written reads writes = 
    let f k v acc = 
        try 
            let _ = Hashtbl.find writes k in 
            acc 
        with Not_found -> k::acc
    in
    let in_args = Hashtbl.fold f reads [] in
    sreg_set_from_list in_args

(* liveness analysis *)
let analyse_liveness instrs = 
    let (reads, writes) = analyse_reads_writes instrs in
    let l2rd = inverse_hash reads in
    let l2wr = inverse_hash writes in
    let in_args = find_read_but_not_written reads writes in
    let cur = hash_get l2wr 0 SRegSet.empty in
    let cur = SRegSet.union cur in_args in
    let _ = Hashtbl.add l2wr 0 cur in
    (* Add a set of live vars to every instruction.
     * il - list of pairs (instr, live_vars)
     * alive - alive vars *)
    let attach instrs = 
        let rec aux (line_no, pairs, alive) instrs =
            match instrs with
            | instr::tl ->
                let new_alive = hash_get l2wr line_no SRegSet.empty in
                let new_dead = hash_get l2rd line_no SRegSet.empty in
                let alive = SRegSet.union (SRegSet.diff alive new_dead) new_alive in
                let pair = (instr, alive) in
                aux (line_no+1, pair::pairs, alive) tl
            | [] -> List.rev pairs
        in
        aux (0, [], SRegSet.empty) instrs
    in
    attach instrs

(* Return a hash: sreg -> set of conflicting sregs.
 * Vars are in conflict, when they can't share a register (they are both alive at the
 * same time).
 *)
let calc_conflicts pairs = 
    let f h (_, alive) = 
        let g acc sreg = 
            let cur = hash_get h sreg SRegSet.empty in
            let new_set = SRegSet.union cur alive in
            Hashtbl.add h sreg new_set
        in
        let elems = SRegSet.elements alive in
        let _ = List.fold_left g () elems in
        h
    in
    let fix h (sreg, set) =
        let set' = SRegSet.remove sreg set in
        let _ = Hashtbl.add h sreg set' in
        h
    in
    let tmp_hash = Hashtbl.create 16 in
    let _ = List.fold_left f tmp_hash pairs in
    let kv_pairs = get_kv tmp_hash in
    let conflicts = Hashtbl.create 16 in
    let conflicts = List.fold_left fix conflicts kv_pairs in
    conflicts

let just_symbolic args = List.filter (function S(_) -> true | _ -> false) args 
let just_concrete args = List.filter (function C(_) -> true | _ -> false) args

let symbolic_args instr =
    let args = arg_dumper instr in
    let args = just_symbolic args in
    args

(* Returns a hash: sreg->set of possible concrete regs *)
let possible_regs possible_regs_by_arg instrs = 
    let analyse_one possible instr =
        let args = symbolic_args instr in
        let f h arg_reg = 
            let regs = possible_regs_by_arg instr arg_reg in
            let cur = 
                try Hashtbl.find h arg_reg 
                with Not_found -> regs
            in
            let cur = RegSet.inter cur regs in
            let _ = Hashtbl.add h arg_reg cur in
            h
        in
        let possible = List.fold_left f possible args in
        possible
    in
    let possible = Hashtbl.create 16 in
    List.fold_left analyse_one possible instrs

let make_assign_regs gmetas stack_ptr frame_ptr =
    let gadgets = get_gadgets gmetas in
    let implement = make_implement stack_ptr frame_ptr in
    let p_by_arg, p_by_pos = make_possible_regs_funs gadgets implement in
    
    let dprintf depth f =
        let pre = String.make (depth*4) ' ' in
        let _ = print_string pre in
        let _ = f () in
        ()
    in
    let rec assign_regs depth instrs top_preserved = 
        (* Make assignments only for symbolic regs *)
        let collect_all_vars instrs = 
            let collect vars instr = 
                let args = symbolic_args instr in
                let args_set = sreg_set_from_list args in
                SRegSet.union vars args_set
            in
            let set = List.fold_left collect SRegSet.empty instrs in
            SRegSet.elements set
        in            
        (* Return all possible assignments of sreg->concrete reg.
         * Return a list of functions sreg->reg *)
        let all_assignments sregs possible conflicts = 
            let rec all_perms f_acc sregs =
                match sregs with
                | sreg::tl ->
                    let p_concrete_set = 
                        try Hashtbl.find possible sreg with Not_found -> assert false
                    in
                    let p_concrete_set = common_reg_set_to_sreg_set p_concrete_set in
                    let conflicting = try Hashtbl.find conflicts sreg with _ -> assert false in
                    (* Collect conflicting regs *)
                    let f sreg acc = 
                        match sreg with
                        | C(creg) -> SRegSet.add sreg acc
                        | S(_) ->
                            begin
                            try 
                                let creg = f_acc sreg in
                                SRegSet.add creg acc
                            with _ -> acc
                            end
                    in
                    let used = SRegSet.fold f conflicting (SRegSet.empty) in
                    let p_concrete_set = SRegSet.diff p_concrete_set used in
                    let p_concrete_list = SRegSet.elements p_concrete_set in
                    let assign_one acc concrete = 
                        let g sr = if sr=sreg then concrete else f_acc sr in
                        let new_perms = all_perms (g) tl in
                        new_perms@acc
                    in
                    List.fold_left assign_one [] p_concrete_list
                | [] -> [f_acc]
            in
            let f_fail r =
                let err = sprintf "Unable to assign to: %s" (dump_sreg r) in
                failwith err
            in
            let perms = all_perms f_fail sregs in
            perms
        in
        (* Apply one of the sregs assignments. Exceptions from this function are
         * always an error.
         * Regs in "alive" sets are also concretized. *)
        let apply_assignment_to_all f_assign pairs = 
            (* Don't throw exceptions on concretized regs *)
            let f_assign = wrap_f_assign f_assign in
            let f acc (instr, alive) = 
                let i = apply_assignment f_assign instr in
                let g x acc = SRegSet.add (f_assign x) acc in
                let alive = SRegSet.fold g alive SRegSet.empty in
                (i,alive)::acc
            in
            let pairs = List.fold_left f [] pairs in
            let pairs = List.rev pairs in
            pairs
        in
        (* Which concrete regs need to be preserved between instructions *)
        let calc_preserved pairs = 
            let f acc (instr, alive) =
                let mod_params = mod_vars instr in
                let mod_params = sreg_set_from_list mod_params in
                (* Instruction can't preserve a param, if it writes to it *)
                let preserved = SRegSet.diff alive mod_params in
                (instr, preserved)::acc
            in
            let pairs = List.fold_left f [] pairs in
            List.rev pairs
        in
        let dump_preserved preserved = 
            let pr sreg = printf "%s;" (dump_sreg sreg) in
            let sregs = SRegSet.elements preserved in
            let _ = dprintf depth (fun _ -> print_string "$ top_preserved: ") in
            let _ = List.map pr sregs in
            dprintf depth (fun _ -> print_newline ())
        in
        (* Check if the assignment is possible.
         * If it is, return instructions paired with gmetas *)
        let satisfy perms pairs = 
            (* Throws Not_found if it's impossible to find a gmeta *)
            let satisfy_t0 instr preserved = 
                let check_possible gmeta =
                    let GMeta(_,_,mod_regs,_) = gmeta in
                    let mod_regs = List.map (fun r -> C(r)) mod_regs in
                    let mod_regs = sreg_set_from_list mod_regs in
                    (*
                    let si = (dump_instr instr) in
                    let _ = dprintf depth (fun _ -> printf "@@ %s, preserved: " si) in
                    let _ = dump_sreg_set preserved in
                    let _ = printf "%s" ", mod_regs: " in
                    let _ = dump_sreg_set mod_regs in
                    let _ = dprintf depth (fun _ -> print_newline ()) in
                    *)
                    let inter = SRegSet.inter preserved mod_regs in
                    (* If the intersection is empty, none of the preserved regs is modified *)
                    SRegSet.is_empty inter 
                in
                let possible_gmetas = try find_all_gmetas instr gmetas with Not_found -> [] in
                (* let _ = dprintf depth (fun _ -> printf "possible gmetas: %d\n" (List.length possible_gmetas)) in *)
                (* Throws Not_found *)
                let gmeta = List.find check_possible possible_gmetas in
                gmeta
            in
            let satisfy_t instr preserved = 
                let impl = implement instr in
                assign_regs (depth+1) impl preserved
            in
            let satisfy_one (instr, preserved) top_preserved = 
                let _ = dprintf depth (fun _ -> printf "implementing %s\n" (dump_instr instr)) in
                let preserved = SRegSet.union preserved top_preserved in
                let typ = instr_type instr in
                let pairs = 
                    match typ with
                    | T0 -> 
                        let gmeta = satisfy_t0 instr preserved in
                        [(instr, gmeta)]
                    | _ -> satisfy_t instr preserved 
                in
                pairs
            in
            let satisfy_all c_pairs top_preserved = 
                let f acc (instr,preserved) =
                    let impl = satisfy_one (instr, preserved) top_preserved in
                    acc@impl
                in
                let impl = List.fold_left f [] c_pairs in
                impl
            in
            (* Test all possible assignments *)
            let rec aux perms =
                match perms with
                | f_assign::tl ->
                    begin
                    (* Concretize all regs *)
                    let c_pairs = apply_assignment_to_all f_assign pairs in
                    (* Remove unnecessary regs from 'alive' set *)
                    let c_pairs = calc_preserved c_pairs in
                    try
                        let impl = satisfy_all c_pairs top_preserved in
                        f_assign, impl
                    with Not_found ->
                        aux tl
                    end
                | [] -> 
                    (* No assignment is satisfiable *)
                    raise Not_found
            in
            let f_assign, impl = aux perms in
            f_assign, impl
        in
        let dump_one_perm vars f_assign = 
            let f sreg = (sreg, f_assign sreg) in
            let l = List.map f vars in
            let pr (s,c) = dprintf depth (fun _ -> printf "%s -> %s\n" (dump_sreg s) (dump_sreg c)) in
            let _ = List.map pr l in
            ()
        in
        (* IN: satisfiable assignment, (instr, gmeta) pairs *)
        let dump_satisfied vars f_assign pairs = 
            let _ = dprintf depth (fun _ -> print_endline "%%% winner assignment:") in
            let _ = dump_one_perm vars f_assign in
            let _ = dprintf depth (fun _ -> print_endline "%%% paired:") in
            let f (instr, gmeta) = 
                let _ = dprintf depth (fun _ -> printf "%s\n" (dump_instr instr)) in
                ()
            in
            let _ = List.map f pairs in
            ()
        in
        let dump vars possible conflicts perms top_preserved pairs =
            let dump_possible possible = 
                let pr (sreg,set) = 
                    let set = common_reg_set_to_sreg_set set in
                    let _ = dprintf depth (fun _ -> printf "%s in {" (dump_sreg sreg)) in
                    let _ = dump_sreg_set set in
                    let _ = dprintf depth (fun _ -> printf "%s\n" "}") in
                    ()
                in
                let _ = dprintf depth (fun _ -> printf "%s\n" "$ possible") in
                let kv = get_kv possible in
                List.map pr kv
            in
            let dump_conflicts h = 
                let _ = dprintf depth (fun _ -> printf "%s\n" "$ conflicts") in
                let kv = get_kv h in
                let pr (k,v) =
                    let _ = dprintf depth (fun _ -> printf "%s conflicts: " (dump_sreg k)) in
                    let _ = dump_sreg_set v in
                    dprintf depth (fun _ -> print_newline ())
                in
                let _ = List.map pr kv in
                ()
            in        
            let dump_perms perms = 
                let _ = dprintf depth (fun _ -> printf "%s\n" "$ perms") in
                let pr perm = 
                    let _ = dump_one_perm vars perm in
                    dprintf depth (fun _ -> print_endline "-")
                in
                let _ = List.map pr perms in
                ()
            in
            let print_pair (instr, alive) = 
                let _ = dprintf depth (fun _ -> Printf.printf "%s alive: " (dump_instr instr)) in
                let _ = dump_sreg_set alive in
                print_newline ()
            in
            let _ = dprintf depth (fun _ -> print_endline "--------------") in
            let _ = List.map print_pair pairs in
            let _ = dump_preserved top_preserved in
            let _ = dump_possible possible in
            let _ = dump_conflicts conflicts in
            let _ = dump_perms perms in
            ()
        in
        let pairs = analyse_liveness instrs in
        let conflicts = calc_conflicts pairs in
        let possible = possible_regs p_by_arg instrs in
        let vars = collect_all_vars instrs in
        let perms = all_assignments vars possible conflicts in  
        let _ = dump vars possible conflicts perms top_preserved pairs in
        let f_assign, s_pairs = satisfy perms pairs in
        let _ = dump_satisfied vars f_assign s_pairs in
        s_pairs
    in
    (* depth = 0 *)
    assign_regs 0
