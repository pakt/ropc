open Printf
open Symbeval
open Int_utils
open Common

let _ = Random.init 0

let oPS = [ADD; SUB; MUL; DIV; XOR; OR; AND]

let gLOBAL_MEM = "mem:?u32"
let sTACK_BASE = 0x0A1D5000
let tOO_BIG = 0x1000 
let eFLAGS = "EFLAGS:u32" 
let eFLAGS_MASK = 0xd5 

let i32 = Int32.of_int
let (!+) = Int32.add 
let (!-) = Int32.sub
let (!*) = Int32.mul
let (!/) = Int32.div
let (!^) = Int32.logxor
let (!|) = Int32.logor
let (!&) = Int32.logand
let (!<<) = Int32.shift_left
let (!>>) = Int32.shift_right

let reg_to_str reg = 
    match reg with
    | EAX -> "EAX"
    | EBX -> "EBX"
    | ECX -> "ECX"
    | EDX -> "EDX"
    | ESI -> "ESI"
    | EDI -> "EDI"
    | EBP -> "EBP"
    | ESP -> "ESP"

let unwrap_ast_var v = 
    match v with
    | Ast.Var(v) -> v
    | _ -> assert false

let str_to_var s = 
    let av,_ = Parser.exp_from_string s in
    let v = unwrap_ast_var av in
    v


let reg_var reg = 
    let reg_s = reg_to_str reg in
    let reg_s = "R_"^reg_s in
    let v = str_to_var reg_s in
    v

let get_gadget gm = 
    match gm with 
    | GMeta(g,_,_,_) -> g

let gmeta_eq g1 g2 = get_gadget g1 = get_gadget g2

let cmp_reg r1 r2 = 
    let s1 = reg_to_str r1 in
    let s2 = reg_to_str r2 in
    String.compare s1 s2

let hash_print h dump = Hashtbl.iter (fun key data -> Printf.printf "%s\n" (dump key data)) h

let rec zip lst1 lst2 = match lst1,lst2 with
  | [],_ -> []
  | _, []-> []
  | (x::xs),(y::ys) -> (x,y) :: (zip xs ys)

let flip pairs = List.map (fun (x,y) -> (y,x)) pairs 

let modified_regs l1 l2 = 
    if List.length l1 <> List.length l2 then assert false else
    let l = zip l1 l2 in
    let f r1 r2 = if r1 <> r2 then assert false else 0 in
    let _ = List.map (fun ((r1,_),(r2,_)) -> f r1 r2) l in (* catch errors *)
    let l = List.filter (fun ((_,v1),(_,v2)) -> v1 <> v2) l in (* find modified *)
    let l = List.map (fun ((r,_),_) -> r) l in
    l

let get_mod_regs rv_pairs rv_new = 
    modified_regs rv_pairs rv_new

let unpack_some l = 
    let rec aux acc l =
        match l with
        | Some(x)::tl -> aux (x::acc) tl
        | None::tl -> aux acc tl
        | [] -> acc
    in
    aux [] l

(* a,e - Ast.Int() *)
let mapmem a e = 
    let t = Typecheck.infer_ast e in
    (* XXX: Fix parser/asmir so that we don't have to do this! *)
    let v, _ = Parser.exp_from_string gLOBAL_MEM in
    let m = unwrap_ast_var v in
    let s = Ast.Move(m, Ast.Store(v, a, e, Ast.exp_false, t), []) in
    s

let mapvar name value = 
    let x = Parser.exp_from_string name in
    let v = 
        match x with
        | Ast.Var(v), _ -> v
        | _ -> failwith "impossible: expected Var"
    in
    let const, _ = Parser.exp_from_string "1:u32" in
    let s = Ast.Move(v, const, []) in 
    s


let set_var var value bits = 
    let v = 
        match bits with
        | 8 -> ast_i8 value
        | 16 -> ast_i16 value
        | 32 -> ast_i32 value
        | _ -> assert false
    in
    let s = Ast.Move(var, v, []) in
    s

let get_var' ctx var f_cast = 
    let delta = ctx.delta in
    let value = Symbeval.Concrete.lookup_var delta var in
    let v = 
        match value with
        | Symbolic(Ast.Int(i,t)) -> f_cast i
        | Symbolic(_) -> assert false 
        | _ -> assert false
    in
    v

let get_var_int ctx var = 
    get_var' ctx var (Big_int.int_of_big_int)

let get_var_int32 ctx var = 
    let f_cast = Int_utils.i32_of_big_int in
    get_var' ctx var f_cast

let set_reg reg value = 
    let r = reg_var reg in
    let s = set_var r value 32 in
    s

let get_reg' ctx reg f_get_var = 
    let rv = reg_var reg in
    let v = f_get_var ctx rv in
    v

let get_reg ctx reg = get_reg' ctx reg get_var_int
let get_reg_int32 ctx reg = get_reg' ctx reg get_var_int32

let rnd_value () = Random.bits () land 0x00FFFFFF 
(* offset, value *)
let gen_dwords num_dwords = 
    let f i = 
        let d = rnd_value () in
        (i*4, d)
    in
    let dwords = Util.mapn f num_dwords in
    dwords

let fill_mem addr dwords = 
    (* generate instructions to fill mem starting @ addr with num_dwords values *)
    let gen_stmts () = 
        let f acc (off, value) = 
            let a = ast_i32 (addr+off) in
            let e = ast_i32 value in
            let s = mapmem a e in
            s::acc
        in
        let stmts = List.fold_left f [] dwords in
        stmts
    in
    let stmts = gen_stmts () in
    stmts

let gen_rv_pairs regs = 
    (* Is there a pair of regs r1,r2 (r1<>r2), such that eax/r1 = eax/r2 ? *)
    let check_div_collision rv_pairs = 
        let p (r,v) = r=EAX in
        let (_,eax) = try List.find p rv_pairs with Not_found -> assert false in
        let rv_pairs = List.filter (fun pair -> not (p pair)) rv_pairs in
        let values = List.map snd rv_pairs in
        let div_values = List.map (fun v -> eax/v) values in
        let div_values = Common.generic_unique div_values in
        List.length values <> List.length div_values
    in
    let rec aux depth = 
        let n_regs = List.length regs in
        let dwords = gen_dwords n_regs in
        let dwords = List.map snd dwords in (* drop offset *)
        let rv_pairs = zip regs dwords in
        (* hack: make EAX bigger, so that EAX/reg is > 0 *)
        let rec fix_eax acc pairs =
            match pairs with
            | (EAX, v)::tl -> fix_eax ((EAX, v lor 0x07000000)::acc) tl
            | hd::tl -> fix_eax (hd::acc) tl
            | [] -> List.rev acc
        in
        let rv_pairs = fix_eax [] rv_pairs in
        let collision = check_div_collision rv_pairs in
        if collision then
            aux (depth+1)
        else
            rv_pairs
    in
    let rv_pairs = aux 0 in
    rv_pairs

let make_hash pairs = 
    let h = Hashtbl.create (List.length pairs) in
    let _ = List.map (fun (k,v) -> Hashtbl.add h k v) pairs in
    h

(* input: hash, list of (reg, value)
 * output: list of (reg, hash[value]) *)
let collect hash pairs = 
    let rec aux acc l = 
        match l with
        | (r,v)::tl -> 
            let org = try Some((r, Hashtbl.find hash v)) with Not_found -> None in
            aux (org::acc) tl
        | [] -> acc
    in
    let reg_value = aux [] pairs in
    unpack_some reg_value

let wrap_meta ctx gadgets mod_regs off_s off_e = 
    (* this can fail if esp has high bits set. reject these cases, 
    since they'd result in too big offsets anyway *)
    let pick_stack_fix sf g = 
        match g with
        | OpEsp(_,_,stack_fix) -> stack_fix
        | _ -> sf
    in
    let esp =
        try
             get_reg ctx ESP 
        with Failure(_) ->
            sTACK_BASE + tOO_BIG + 1
    in
    let stack_fix = esp - sTACK_BASE in
    let sf = 
        match gadgets with
        | OpEsp(_,_,fix)::tl -> fix
        | _ -> stack_fix 
    in
    let wm gadget = 
        let fm = FileMeta(off_s, off_e) in
        let gm = GMeta(gadget, fm, mod_regs, sf) in 
        gm
    in
    if sf > tOO_BIG || sf < 4 then 
        []
    else
        List.map wm gadgets 

(* return all ordered pairs of l1 x l2 *)
let combos l1 l2 = 
    let rec aux acc l' = 
        match l' with
        | hd::tl ->  
            let pairs = List.map (fun x->(hd,x)) l2 in
            aux (pairs::acc) tl
        | [] -> acc
    in
    let ll = aux [] l1 in
    List.concat ll

let gen_init_and_rv_pairs' f_cast regs = 
    let rv_pairs = gen_rv_pairs regs in
    let init = List.map (fun (r,v) -> set_reg r v) rv_pairs in
    let rv_pairs = List.map (fun (r,v) -> (r, f_cast v)) rv_pairs in
    (rv_pairs, init)

let gen_init_and_rv_pairs regs = gen_init_and_rv_pairs' (fun x->x) regs

let gen_init_and_rv_pairs32 regs = 
    let f_cast = Int32.of_int in
    gen_init_and_rv_pairs' f_cast regs

let gen_init_and_rv_pairs32_no_esp () = gen_init_and_rv_pairs32 Common.rEGS_NO_ESP

let get_regs_from_ctx ctx regs = 
    let rv = List.map (fun r -> (r, get_reg_int32 ctx r)) regs in
    rv

let get_regs_no_esp ctx = get_regs_from_ctx ctx Common.rEGS_NO_ESP

let common_init stmts = 
    let init_esp = set_reg ESP sTACK_BASE in
    [init_esp]

(*
 * procedures for gadget recognition
 *)
let g_load_const () = 
    let num_dwords = 8 in
    let dwords = gen_dwords num_dwords in
    let dwords32 = List.map (fun (off,dw) -> (off, Int32.of_int dw)) dwords in
    (* we want to know which regs were trashed *)
    let (rv_pairs, regs_init) = gen_init_and_rv_pairs32_no_esp () in
    (* generate ctx.delta with filled memory *)
    let f_init stmts = 
        let init = fill_mem sTACK_BASE dwords in
        (* we are passing rv_pairs outside to lower redundancy: modified regs will be detected by gadget.ml *)
        regs_init @ init
    in
    let f_finish ctx = 
        let dwords = dwords32 in
        let find_dword v = 
            let p (off, dw) = dw = v in
            let x = 
                try Some(List.find p dwords)
                with Not_found -> None
            in
            x
        in
        let print (r,v) = 
            let sr = reg_to_str r in
            (* let _ = Printf.printf "%s = 0x%08lx\n" sr v in *)
            ()
        in
        let rv_new = get_regs_no_esp ctx in
        let _ = List.map print rv_new in
        let _ = flush stderr in
        let _ = flush stdout in
        (* let rv_pairs = zip Common.rEGS values in *)
        let collect rv_list = 
            let rec aux acc l = 
                match l with
                | (r, Some((off, dw)))::tl -> aux ((r, off)::acc) tl
                | (r, None)::tl -> aux acc tl
                | [] -> acc
            in
            aux [] rv_list
        in
        let l = List.map (fun (r,v) -> (r, find_dword v)) rv_new in
        let reg_off_pairs = collect l in
        let gadgets = List.map (fun (r,o) -> LoadConst(r, o)) reg_off_pairs in
        gadgets, (modified_regs rv_pairs rv_new)
    in
    let mem_hax = false in
    let repeats = 1 in
    (f_init, f_finish, mem_hax, repeats)

let g_copy_reg () = 
    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in
    let rv_hash = make_hash (flip rv_pairs) in
    (* HAX: We want to detect reg <- esp *)
    let _ = Hashtbl.add rv_hash (i32 sTACK_BASE) ESP in

    let f_init stmts = 
        init
    in

    let f_finish ctx = 
        let rv_new = get_regs_no_esp ctx in
        let dst_src = collect rv_hash rv_new in
        (* delete trivial copies *)
        let dst_src = List.filter (fun (r1,r2) -> r1 <> r2) dst_src in
        let gadgets = List.map (fun (dst,src) -> CopyReg(dst, src)) dst_src in
        (* let _ = printf "$$$ found: %d\n" (List.length gadgets) in *)
        gadgets, (modified_regs rv_pairs rv_new)
    in
    let mem_hax = false in
    let repeats = 1 in
    (f_init, f_finish, mem_hax, repeats)

let calc_op v1 op v2 = 
    match op with
    | ADD -> !+ v1 v2
    | SUB -> !- v1 v2
    | MUL -> !* v1 v2
    | DIV -> !/ v1 v2
    | AND -> !& v1 v2
    | OR  -> !| v1 v2
    | XOR -> !^ v1 v2

let calc_pair ((r1,v1),(r2,v2)) = 
    let f op = 
        let v = calc_op v1 op v2 in
        (v,r1,op,r2)
    in
    List.map f oPS

(* FIXME: division is incorrect (should handle 64bits *)
let g_binop () = 
    (* input: list of (r1,v1),(r2,v2) pairs *)
    let calc_all l = 
        (* we don't want to have both BinOp(_,eax,+,edx) and BinOp(_,edx,+,eax) *)
        let filter_commutative  = 
            let is_commutative op = 
                match op with
                | ADD | MUL | AND | OR | XOR -> true
                | SUB | DIV -> false
            in
            let p (_,r1,op,r2) = 
                if is_commutative op then
                    if cmp_reg r1 r2 = -1 then true else false
                else
                    true
            in
            p
        in
        (* FIXME?: overapproximated for now *)
        let filter_trivial = 
            let is_trivial (v,r1,op,r2) = 
                (* only EAX=EAX/reg *)
                let bad_div = op = DIV && ((v = Int32.zero)  || r1 <> EAX) in
                bad_div || r1 = r2
            in
            (fun x -> not (is_trivial x))
        in
        let dump l = 
            (* let pr (v,r1,op,r2) = Printf.printf "$ %d %s %s %s\n"  (Int32.to_int v)
            (Common.dump_reg r1) (Common.dump_op op) (Common.dump_reg r2) 
            in
            List.map pr l
            *)
            ()
        in
        let calculated = List.map calc_pair l in
        (* (i32 value, reg1, op, reg2) *)
        let calculated = List.concat calculated in
        let calculated = List.filter filter_commutative calculated in
        let calculated = List.filter filter_trivial calculated in
        (* ESP is just a dummy value and needs to be replaced later *)
        let wrap (v,r1,op,r2) = BinOp(ESP,r1,op,r2) in
        let pairs = List.map (fun x -> let (v,_,_,_) = x in (v, wrap x)) calculated in
        let cmp (v1,_) (v2,_) = Int32.compare v1 v2 in
        (* ops with equal results are near each other *)
        let pairs = List.sort cmp pairs in
        (* [(v0,_);(v0,_);(v1,_);(v2,_);(v2,_);(v2,_)] -> [(v0,l0);(v1,l1);(v2,l2)] *)
        let gather pairs = 
            let rec aux acc cur_v cur_l l = 
                match l with
                | hd::tl ->
                    let v = fst hd in
                    let x = snd hd in
                    if v = cur_v then
                        aux acc cur_v (x::cur_l) tl
                    else
                        aux ((cur_v,cur_l)::acc) v [x] tl
                | [] -> 
                    if cur_l = [] then acc
                    else ((cur_v,cur_l)::acc)
            in
            let partitions = 
                match pairs with
                | [] -> []
                | hd::tl -> 
                    let (v,x) = hd in
                    aux [] v [x] tl
            in
            partitions
        in
        let partitions = gather pairs in
        let h = make_hash partitions in
        h
    in

    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in
    let rvrv = combos rv_pairs rv_pairs in
    (* int32 value -> r1,op,r2 resulting in that value *)
    let value_hash = calc_all rvrv in

    let f_init stmts = 
        init
    in
    let f_finish ctx = 
        let rv_new = get_regs_no_esp ctx in
        (* get a list of (reg, all posible binops resulting in value found in reg) *)
        let r_binop_l = collect value_hash rv_new in
        let set_dst_in_binops (r, binops) = 
            let f binop = 
                match binop with
                | BinOp(fake_dst,r1,op,r2) ->
                    if fake_dst <> ESP then assert false
                    else BinOp(r,r1,op,r2)
                | _ -> assert false
            in
            let binops = List.map f binops in
            binops
        in
        let binops = List.map set_dst_in_binops r_binop_l in
        let gadgets = List.concat binops in
        gadgets, (modified_regs rv_pairs rv_new)
                        
    in
    let mem_hax = false in
    let repeats = 1 in
    (f_init, f_finish, mem_hax, repeats)

let get_global_mem_var () = 
    let ast_memv,_ = Parser.exp_from_string gLOBAL_MEM in
    let memv = unwrap_ast_var ast_memv in
    memv

let get_global_mem ctx = 
    let delta = ctx.delta in
    let memv = get_global_mem_var () in
    try
        let mem = Symbeval.VH.find delta memv in
        Some(mem)
    with Not_found ->
        None

let ast_int_to_byte32 v = 
    let byte = 
        match v with
        | Ast.Int(v',_) -> Big_int.int32_of_big_int v' 
        | _ -> assert false
    in
    if byte < (i32 0) || byte > (i32 255) then 
        assert false
    else
        byte

(* input: addr (int32)
 * output: byte (int32) under addr in global mem  *)
let get_mem_byte mem addr = 
    let addr = Ast.Int(Big_int.big_int_of_int32 addr, Type.Reg 32) in
    let v = Symbeval.Concrete.lookup_mem mem addr Ast.exp_true in (* Ast.exp_true = little endian *)
    ast_int_to_byte32 v

(*
let bswap v32 = 
    let get_byte v i = 
        let b = (!&) ((!>>) v (i*8)) 0xFF
*)

let get_mem_dword mem addr = 
    let bytes_in_dword = 4 in
    let f i = 
        let addr = (!+) addr (i32 i) in
        get_mem_byte mem addr
    in
    let bytes = Util.mapn f (bytes_in_dword-1) in 
    let glue acc b = (!+) ((!<<) acc 8) b in
    (* little endian *)
    let bytes = List.rev bytes in 
    let dword = List.fold_left glue (i32 0) bytes in
    dword

(* returns a list of addresses with defined bytes *)
let enumerate_mem mem = 
    let mem_map = 
        match mem with
        | ConcreteMem(mem_map,_) -> mem_map
        | _ -> assert false
    in
    let f k v acc = 
        let k = int64_to_int32 k in
        k::acc
    in
    let addrs = Symbeval.AddrMap.fold f mem_map [] in
    List.sort compare addrs

let get_av_pairs_mem mem = 
    let addrs = enumerate_mem mem in
    let get_dword acc addr = 
        let dw = get_mem_dword mem addr in
        (addr, dw)::acc
    in
    let av_pairs = List.fold_left get_dword [] addrs in
    let print (a,dw) = 
        let dw = Int32.to_int dw in
        let a = Int32.to_int a in
        (* let _ = Printf.printf "$$$ [0x%08x] = 0x%08x\n" a dw in *)
        ()
    in
    let _ = List.map print av_pairs in
    av_pairs

let get_av_pairs_mem' ctx f_mem_get = 
    let memv = f_mem_get ctx in
    match memv with
    | Some(memv) -> get_av_pairs_mem memv
    | None -> []

let get_av_pairs_mem_init ctx = get_av_pairs_mem' ctx get_global_mem

let get_av_pairs_mem_uninit ctx = 
    let f_mem_get ctx = 
        let u_mems = ctx.u_mems in
        let memv = get_global_mem_var () in
        try
            let umem = Symbeval.VH.find u_mems memv in
            Some(umem)
        with Not_found ->
            None
    in
    get_av_pairs_mem' ctx f_mem_get

(* returns a list of (addr, reg) *)
let match_value_to_reg' f_eq f_pack av_pairs rv_pairs =
    let rec aux acc l = 
        match l with
        | hd::tl -> 
            begin
            try
                let eq = f_eq hd in
                let elem = List.find eq rv_pairs in
                let newhd = f_pack hd elem in
                aux (newhd::acc) tl
            with Not_found ->
                aux acc tl
            end
        | [] -> acc
    in
    let ar_pairs = aux [] av_pairs in
    ar_pairs

let match_value_to_reg l1 l2 = 
   let f_eq = (fun (addr_,av) (reg,rv) -> av=rv) in
   let f_pack = (fun (addr_,v) (reg,_) -> (addr_, reg)) in
   match_value_to_reg' f_eq f_pack l1 l2
    
(* returns a list of (addr_reg, offset, reg) *)
let addr_to_reg_plus_offset ar_pairs rv_pairs = 
    let rec aux acc l = 
        match l with
        | (addr, reg)::tl ->
            let ro_pairs = List.map (fun (reg, rv) -> (reg, (!-) addr rv)) rv_pairs in
            let addr_off_src = List.map (fun (addr_reg, off) -> (addr_reg, off, reg)) ro_pairs in
            aux (addr_off_src::acc) tl
        | [] -> acc
    in
    (* (addr_reg, offset, src_reg) *)
    let ros = aux [] ar_pairs in
    List.concat ros

let g_write_mem () = 
    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in

    let f_init stmts = 
        init
    in
    let f_finish ctx = 
        let av_pairs = get_av_pairs_mem_init ctx in
        let ar_pairs = match_value_to_reg av_pairs rv_pairs in
        let ar_off_src = addr_to_reg_plus_offset ar_pairs rv_pairs in
        let gadgets = List.map (fun (addr_reg, off, src_reg) -> WriteMem(addr_reg, off, src_reg)) ar_off_src in
        let rv_new = get_regs_no_esp ctx in
        gadgets, (modified_regs rv_pairs rv_new)
    in
    (* this is to prevent write_mem_op being recognized as write_mem.
       without memhax, uninitialized memory = 0, so xor [reg+off], reg' can
       be interpreted as mov [reg+off], reg' *)
    let mem_hax = true in
    let repeats = 2 in (* there are many possibilities for addr_reg+offset *)
    (f_init, f_finish, mem_hax, repeats)

let g_read_mem () = 
    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in

    let f_init stmts = 
        init
    in

    let f_finish ctx = 
        let av_pairs = get_av_pairs_mem_uninit ctx in
        let rv_new = get_regs_no_esp ctx in
        let ar_pairs = match_value_to_reg av_pairs rv_new in
        let ar_off_dst = addr_to_reg_plus_offset ar_pairs rv_pairs in
        let gadgets = List.map (fun (addr_reg, off, dst_reg) -> ReadMem(dst_reg, addr_reg, off)) ar_off_dst in
        let rv_new = get_regs_no_esp ctx in
        gadgets, (modified_regs rv_pairs rv_new)
    in
    let mem_hax = true in
    let repeats = 2 in
    (f_init, f_finish, mem_hax, repeats)

let g_read_mem_op () = 
    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in

    let f_init stmts = 
        init
    in

    let f_finish ctx = 
        (* allowing 0/1 will produce many false positives, like reg = reg * 1, reg=reg+0 *)
        let good_const v = (!&) v (i32 0x0fffff00) <> (i32 0) in
        let rv_new = get_regs_no_esp ctx in
        let av_pairs = get_av_pairs_mem_uninit ctx in
        let av_pairs = List.filter (fun (a,v) -> good_const v) av_pairs in
        let rv_av = combos rv_pairs av_pairs in
        (* (i32,reg,op,address) list *)
        let calculated = List.map calc_pair rv_av in
        let calculated = List.concat calculated in
        let l = List.map (fun (i,r,op,addr) -> ((r,op,addr),i)) calculated in
        let l = match_value_to_reg l rv_new in
        let matched = List.map (fun ((r_old,op,addr),r_new) -> (r_new, r_old, op, addr)) l in
        let matched = List.filter (fun (r_new, r_old, _, _) -> r_new = r_old) matched in
        let l = List.map (fun (r,_,op,addr) -> (addr, (r,op))) matched in
        let l = addr_to_reg_plus_offset l rv_pairs in
        let l = List.map (fun (addr_reg, off, (r,op)) -> (r,op,addr_reg,off)) l in
        let gadgets = List.map (fun (r_dst, op, addr_reg, off) -> ReadMemOp(r_dst, op, addr_reg, off)) l in
        gadgets, (modified_regs rv_pairs rv_new)
    in
    let mem_hax = true in
    let repeats = 2 in
    (f_init, f_finish, mem_hax, repeats)

let g_write_mem_op () = 
    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in

    let f_init stmts = 
        init
    in
    let f_finish ctx = 
        (* allowing 0/1 will produce many false positives, like reg = reg * 1, reg=reg+0 *)
        let good_const v = (!&) v (i32 0x0fffff00) <> (i32 0) in

        let undef_av = get_av_pairs_mem_uninit ctx in
        let def_av = get_av_pairs_mem_init ctx in
        let f_eq (a1,_) (a2,_) = a1=a2 in
        let f_pack (a,v1) (_,v2) = (a,v1,v2) in
        (* (addr, before value, after value) list *)
        let avv = match_value_to_reg' f_eq f_pack undef_av def_av in
        (* if value didn't change, then most likely no arith. op was performed *)
        let avv = List.filter (fun (a,v1,v2) -> v1 <> v2) avv in
        let avv = List.filter (fun (a,v1,v2) -> good_const v1 && good_const v2) avv in
        let calc (a,v_before,v_after) =
            let f (reg,rv) = 
                let g op = 
                    let v = calc_op v_before op rv in
                    (a,v_before,reg,op,v_after,v)
                in
                List.map g oPS
            in
            let l = List.map f rv_pairs in
            let l = List.concat l in
            let l = List.filter (fun (a,v1,r,op,v2,v3) -> v2 = v3) l in
            l
        in
        let l = List.map calc avv in
        let l = List.concat l in
        let l = List.map (fun (a,_,reg,op,_,_) -> (a, (reg,op))) l in
        let l = addr_to_reg_plus_offset l rv_pairs in
        let l = List.map (fun (addr_reg, off, (reg,op)) -> (addr_reg, off, op, reg)) l in
        let gadgets = List.map (fun (addr_reg, off, op, reg) -> WriteMemOp(addr_reg, off, op, reg)) l in
        let rv_new = get_regs_no_esp ctx in
        gadgets, (modified_regs rv_pairs rv_new)
    in
    let mem_hax = true in
    let repeats = 2 in
    (f_init, f_finish, mem_hax, repeats)

(* AH := SF:ZF:xx:AF:xx:PF:1:CF;
 * xx - unknown
 * mask: 0xd5
 * 2nd youngest bit of EFLAGS is set to 1 (reserved bit) *)
let g_lahf () = 
    let eflags = str_to_var eFLAGS in
    let init_eflags () = 
        let efl_val = rnd_value () in
        let set_efl = set_var eflags efl_val 32 in
        set_efl
    in
    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in
    let set_efl = init_eflags () in
    let init = set_efl::init in

    let f_init stmts = 
        init
    in
    let f_finish ctx = 
        let mask_efl v = 
            let v = (v land eFLAGS_MASK) lor 2 in 
            v
        in
        let eflv = get_var_int ctx eflags in
        let eflv = mask_efl eflv in
        (* 2nd bit of eflags is always set *)
        let eaxv = get_reg_int32 ctx EAX in
        let eaxv = Int32.to_int ((!>>) eaxv 8) in
        let ah = mask_efl (eaxv land 0xFF) in
        let gadgets = 
            if eflv = ah then [Lahf]
            else []
        in
        let rv_new = get_regs_no_esp ctx in
        gadgets, (modified_regs rv_pairs rv_new)
    in
    let mem_hax = false in
    let repeats = 2 in
    (f_init, f_finish, mem_hax, repeats)

let g_opesp () = 
    let (rv_pairs, init) = gen_init_and_rv_pairs32_no_esp () in
    let f_init stmts = 
        init
    in
    let f_finish ctx = 
        let old_esp = i32 sTACK_BASE in
        let new_esp = get_reg_int32 ctx ESP in
        let rv_new = get_regs_no_esp ctx in
        (* we care only about esp = esp +- reg *)
        let ops = [ADD; SUB] in
        let calc v1 (r,v2) = 
            List.map (fun op -> (calc_op v1 op v2, op, r)) ops 
        in
        let f acc (r,v) = 
            let l = calc old_esp (r,v) in
            l::acc
        in
        (* (v,op,reg) where v = old_esp op reg *)
        let l = List.fold_left f [] rv_pairs in
        let l = List.concat l in
        (* (stack_fix,op,reg) *)
        let l = List.map (fun (v,op,r) -> ((!-) new_esp v, op, r)) l in
        let l = List.map (fun (v,op,r) -> (Int32.to_int v,op,r)) l in
        (* filter impossible stack_fixes *)
        let l = List.filter (fun (sf,op,r) -> sf>=4 && sf < tOO_BIG) l in
        let gadgets = List.map (fun (sf,op,r) -> OpEsp(op, r, sf)) l in
        gadgets, (modified_regs rv_pairs rv_new)
    in
    let mem_hax = false in
    let repeats = 2 in
    (f_init, f_finish, mem_hax, repeats)

let g_list () = 
    [
        g_copy_reg;
        g_binop;
        g_load_const;
        g_write_mem;
        g_read_mem;
        g_read_mem_op;
        g_write_mem_op;
        g_lahf;
        g_opesp;
    ]

