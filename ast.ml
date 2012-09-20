open Printf
open Common

type meta = {lnum: int;}
type 'a wrapped = {n: 'a; m: meta}

type id = string
type operator = Add | Sub | Mul | Div | Xor | And | Or | Not
type exp = 
      BinOp of exp * operator * exp
    | UnOp of operator * exp
    | Var of id
    | Ref of id (* address of var *)
    | ReadMem of id
    | Const of int

type flag = E | A | B | MP (* jMP = jump always *)
type cond = Cond of flag list | NCond of flag list
type exp_args = ExpArgs of exp list

type stmt = 
    | Assign of id * exp
    | DerefAssign of id * exp (* *var = 1 *)
    | AssignTab of id * int list (* var = [1,2,3] *)
    | WriteMem of id * exp
    | Branch of cond * id
    | Label of id
    | Cmp of exp * exp
    | Call of id * exp_args
    | ExtCall of id * exp_args
    | Enter of int
    | Leave
    | Ret of id

type args = Args of id list
type func_body = FunBody of stmt list
type func = Fun of id * args * func_body
type program = Prog of func list

(* bad but simple *)
type func_body' = FunBody' of stmt wrapped list
type func' = Fun' of id * args * func_body'
type program' = Prog' of func' wrapped list

type error = Error of meta * string

let unwrap wrapped = wrapped.n
let get_meta wrapped = wrapped.m

let unwrap_func func = 
    let func = unwrap func in
    let Fun'(id, args, FunBody'(sl)) = func in
    let sl = List.map unwrap sl in
    Fun(id, args, FunBody(sl))

let unwrap_prog p = 
    let Prog'(fl) = p in
    let fl = List.map unwrap_func fl in
    Prog(fl)

let br = ["e";"a";"b";]
let branches = br @ (List.map (fun x->"n"^x) br) @ ["mp"]

let fl_to_char = [(E, 'e'); (A, 'a'); (B, 'b');(MP, '@')]
let ch_to_flag = List.map (fun (x,y)->(y,x)) fl_to_char
let f2c = Common.create_hashtable 8 fl_to_char
let c2f = Common.create_hashtable 8 ch_to_flag

let str_to_cond s = 
    let char_to_flag c = 
      try
        let flag = Hashtbl.find c2f c in
        flag
      with Not_found ->
        assert false
    in
    let str_to_flag_list s = 
        let rec aux s i n acc = 
            if i == n then 
                acc
            else
                let c = s.[i] in
                match c with
                | 'n' -> assert false
                | _ -> 
                    let f = char_to_flag c in
                    aux s (i+1) n (f::acc)
        in
        aux s 0 (String.length s) []
    in

    if s="mp" then Cond([MP]) 
    else

    let tl = String.sub s 1 (String.length s -1) in
    let hd = s.[0] in
    (* let _ = Printf.printf "strtocond: %s\n" (string_of_int (List.length fl)) in *)
    match hd with
    | 'n' -> 
        let fl = str_to_flag_list tl in
        NCond(fl)
    | _ -> 
        let fl = str_to_flag_list s in
        Cond(fl)
 
let str_fold f l sep = 
    let s = List.fold_left (fun acc s -> acc ^ f s ^ sep) "" l in
    if String.length s > 0 then
        String.sub s 0 (String.length s - 1)
    else 
        s
 
let id = fun x->x

let dump_op op =
    match op with 
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Xor -> "^"
    | And -> "&"
    | Or -> "|"
    | Not -> "~"

let rec dump_exp exp =
    match exp with
    | Const x -> string_of_int x
    | Var x -> x
    | Ref x -> Printf.sprintf "&%s" x
    | UnOp(op, e) -> Printf.sprintf "UnOp(%s, %s)" (dump_op op) (dump_exp e)
    | BinOp(e1, op, e2) -> Printf.sprintf "BinOp(%s,%s,%s)" (dump_exp e1) (dump_op op) (dump_exp e2) 
    | ReadMem(id) -> Printf.sprintf "ReadMem(%s)" id

let dump_exp_args ea = 
    match ea with
    | ExpArgs(args) -> 
        let s_args = str_fold dump_exp args "," in
        s_args

let dump_flag_list ll =
    let rec aux l acc = 
        match l with 
        | hd::tl -> 
            begin
              try
                let c = Hashtbl.find f2c hd in
                aux tl (c::acc)
              with Not_found ->
                assert false
            end
        | [] -> acc
    in
    let chars = aux ll [] in
    let strs = List.map (fun c -> String.make 1 c) chars in
    let s = str_fold id strs ";" in
    s

let dump_cond cond = 
    match cond with
    | Cond(l) -> 
        let s = dump_flag_list l in
        Printf.sprintf "[%s]" s
    | NCond(l) -> 
        let s = dump_flag_list l in
        Printf.sprintf "~[%s]" s

let rec dump_stmt stmt = 
    match stmt with
    | Assign(id, exp) -> Printf.sprintf "Assign(%s, %s)" id (dump_exp exp)
    | DerefAssign(id, exp) -> Printf.sprintf "DerefAssign(%s, %s)" id (dump_exp exp)
    | AssignTab(id, l) -> Printf.sprintf "AssignTab(%s, %s)" id (dump_int_list l)
    | Branch(cond_l, id) -> Printf.sprintf "Branch(%s, %s)" (dump_cond cond_l) id
    | Label(id) -> Printf.sprintf "Label(%s)" id
    | WriteMem(id, exp) -> Printf.sprintf "WriteMem(%s, %s)" id (dump_exp exp)
    | Cmp(e1, e2) -> Printf.sprintf "Cmp(%s, %s)" (dump_exp e1) (dump_exp e2)
    | Call(id, exp_args) -> Printf.sprintf "Call(%s, %s)" id (dump_exp_args exp_args)
    | ExtCall(id, exp_args) -> Printf.sprintf "!Call(%s, %s)" id (dump_exp_args exp_args)
    | Enter(n) -> Printf.sprintf "Enter(%d)" n
    | Leave -> "Leave"
    | Ret(s) -> Printf.sprintf "ret(%s)" s

let dump_args args = 
    match args with
    | Args(args) ->
        let s_args = str_fold id args "," in
        s_args

let dump_body body =
    match body with
    | FunBody(stmt_list) -> str_fold dump_stmt stmt_list "\n"

let dump_func_head f = 
    match f with 
    | Fun(id, args, _) ->
        let s_args = dump_args args in
        let s = Printf.sprintf "# Fun: %s, args: %s" id s_args in
        s

let dump_func f =
    match f with 
    | Fun(id, args, body) ->
        let head = dump_func_head f in
        let s_body = dump_body body in
        let s = Printf.sprintf "%s\n%s\n" head s_body in
        s

let dump_prog p = 
    match p with 
    | Prog(func_list) -> str_fold dump_func func_list "\n"

(* AST verification 
 * - are all vars initialized before use? (done)
 * - branches only to defined labels (done)
 * - calls only to defined functions (done)
 * - unique function names (done)
 * - exactly one "main" function without (?) params (done)
 * - calls with correct number of params (done)
 *)

let is_label x = 
    match x with 
    | Label(id) -> true
    | _ -> false

let is_fun_wr x = 
    match x with 
    | Fun'(id,_,_) -> true

let is_branch x =
    match x with
    | Branch(_, id) -> true
    | _ -> false

let is_call = function
    | Call(id, _) -> true
    | _ -> false

let is_ext_call = function
    | ExtCall(id, _) -> true
    | _ -> false

let is_init_id = function
    | Assign(id, _) -> true, id
    | _ -> false, "NOT AN ID"


let make_collect is_thing stmts = 
    let rec aux stmts acc = 
        match stmts with
        | hd::tl -> 
            let ok = is_thing hd in
            if ok then 
                aux tl (hd::acc)
            else    
                aux tl acc
        | [] -> acc
    in
    aux stmts []

let make_collect_wr is_thing stmts = 
    let is_thing = fun x -> is_thing (unwrap x) in
    make_collect is_thing stmts

let collect_labels_wr = make_collect_wr is_label
let collect_branches_wr = make_collect_wr is_branch
let collect_calls_wr = make_collect_wr is_call

(*
let collect_call_targets = make_collect is_call_target
let collect_ext_call_targets = make_collect is_ext_call_target
*)

let collect_used_vars_exp exp = 
    let rec aux exp acc = 
        match exp with
        | Const _ | ReadMem _ -> acc
        | Var _ | Ref _ -> exp::acc
        | UnOp(_,e) -> aux e acc
        | BinOp(e1,_,e2) -> 
            let acc = aux e1 acc in
            aux e2 acc
    in
    aux exp []        

let update_init_vars vars stmt = 
    let ok, id = is_init_id stmt in
    if ok then id::vars
    else vars

let collect_used_vars_stmt = function
    | Assign(_, e) 
    | DerefAssign(_, e) 
    | WriteMem(_, e) -> collect_used_vars_exp e
    | Cmp(e1, e2) ->
        let l1 = collect_used_vars_exp e1 in
        let l2 = collect_used_vars_exp e2 in
        l1 @ l2
    | Call(_, e_args) | ExtCall(_, e_args) -> 
        let f acc exp = 
            let vars = collect_used_vars_exp exp in
            vars :: acc
        in
        let ExpArgs(expl) = e_args in
        let ll = List.fold_left f [] expl in
        List.concat ll
    | _ -> []
    (*
    | Branch of cond * id
    | Label of id
    *)

let collect_init_var = function
    | AssignTab(v,_) 
    | Assign(v,_) -> Some(v)
    | _ -> None

let var_id hd =    
    let id = match hd with Ref(id) | Var(id) -> id | _ -> assert false in
    id

let label_id = function
    | Label(id) -> id
    | _ -> assert false

let call_id = function
    | Call(id, _) -> id
    | _ -> assert false

let fun_id' = function
    | Fun'(id, _, _) -> id

let make_error_fun f l = 
    let aux acc node =
        let pos, s = f node in
        let e = Error(pos, s) in
        e::acc
    in
    List.fold_left aux [] l

let fancy_filter_ f g defs nodes flip = 
    let p node = 
        let pred = fun x -> f x = g node in
        let found = (List.exists pred defs) in
        if flip then
            not found
        else
            found
    in
    List.filter p nodes

(* return nodes not 'defined' in defs *)
let fancy_filter f g defs nodes = fancy_filter_ f g defs nodes true

(* return nodes 'defined' in defs *)
let fancy_filter' f g defs nodes = fancy_filter_ f g defs nodes false


let used_before_init init_vars vars = 
    let f = var_id in
    let bad = fancy_filter id f init_vars vars in
    bad

let verify_vars_in_func func =
    let error_not_init pos vars = 
        let f var = 
            let id = var_id var in
            let s = Printf.sprintf "Uninitialized variable: %s" id in
            (pos, s)
        in
        let erf = make_error_fun f in
        erf vars
    in
    let Fun'(id, Args(args), FunBody'(stmts)) = func in
    (* uninit. ids are reported only once *)
    let find_uninitialized stmts = 
        let rec aux stmts init_vars errors = 
            match stmts with
            | hd::tl ->
                begin
                let pos = get_meta hd in
                let hd = unwrap hd in
                let vars = collect_used_vars_stmt hd in
                let new_init = collect_init_var hd in (* Some(v) / None *)
                let bad = used_before_init init_vars vars in
                let new_errors = error_not_init pos bad in
                let errors = new_errors @ errors in
                match new_init with
                | Some(v) -> aux tl (v::init_vars) errors
                | None -> aux tl init_vars errors
                end 
            | [] -> errors
        in
        aux stmts args []
    in
    find_uninitialized stmts

let verify_jumps_in_func func = 
    let branch_target branch =
        match branch with
        | Branch(_, t) -> t
        | _ -> assert false
    in
    let error_bad_label branches = 
        let f branch = 
            let pos = get_meta branch in
            let branch = unwrap branch in
            let id = branch_target branch in
            let s = Printf.sprintf "No such label: %s" id in
            (pos, s)
        in
        let erf = make_error_fun f in
        erf branches
    in
    let Fun'(id, args, FunBody'(stmts)) = func in
    let def_labels = collect_labels_wr stmts in
    let branches = collect_branches_wr stmts in
    let f label = label_id (unwrap label) in
    let g branch = branch_target (unwrap branch) in
    let bad = fancy_filter f g def_labels branches in
    error_bad_label bad
            
let cmp_by_pos e1 e2 =
    let Error(p1, _) = e1 in
    let Error(p2, _) = e2 in
    p1.lnum - p2.lnum

let cmp_by_str e1 e2 = 
    let Error(_, s1) = e1 in
    let Error(_, s2) = e2 in
    if s1 < s2 then -1
    else
        if s1 = s2 then 0
        else 1

let sort_by_pos errors = 
    List.sort cmp_by_pos errors 

let sort_and_cut errors = 
    let errors = sort_by_pos errors in
    let errors = List.stable_sort cmp_by_str errors in
    let errors = Common.unique cmp_by_str errors in
    let errors = sort_by_pos errors in
    errors

let pos_id_call call = 
    let pos = get_meta call in
    let id = call_id (unwrap call) in
    (pos, id)

let collect_calls_in_func func = 
    let Fun'(_, _, FunBody'(stmts)) = func in 
    let calls = collect_calls_wr stmts in
    calls

let verify_calls f_ids func = 
    let calls = collect_calls_in_func func in
    let f call = 
        let (pos, id) = pos_id_call call in
        let s = Printf.sprintf "No such function: %s" id in
        (pos, s)
    in
    let g call = call_id (unwrap call) in
    let bad_calls = fancy_filter id g f_ids calls in
    let erf = make_error_fun f in
    erf bad_calls

(* id_count = [(f_id, f_param_count); ...] *)
let verify_calls_params id_count func = 
    let call_arg_count call = 
        match call with
        | Call(id, ExpArgs(l)) -> List.length l
        | _ -> assert false
    in
    let calls = collect_calls_in_func func in
    let f call = 
        let (pos, id) = pos_id_call call in
        let c_count = call_arg_count (unwrap call) in
        let (_, f_count) = List.find (fun (f_id, _) -> f_id = id) id_count in
        let s = Printf.sprintf "Function \"%s\" takes %d params, not %d" id f_count c_count in
        (pos, s)
    in
    let g call = 
        let call = (unwrap call) in
        let id = call_id call in
        let c_count = call_arg_count call in
        (id, c_count)
    in
    let g_id call = call_id (unwrap call) in
    let defined_calls = fancy_filter' fst g_id id_count calls in
    let bad = fancy_filter id g id_count defined_calls in
    let erf = make_error_fun f in
    erf bad

let cmp_func_by_pos f1 f2 = 
    let m1 = get_meta f1 in
    let m2 = get_meta f2 in
    m1.lnum - m2.lnum

let cmp_func_by_id f1 f2 = 
    let f1 = unwrap f1 in
    let f2 = unwrap f2 in
    let Fun'(id1, _, _) = f1 in
    let Fun'(id2, _, _) = f2 in
    if id1 = id2 then 0 
    else 
        if id1 < id2 then (-1) else 1

let verify_funs f_list = 
    let f_list = List.sort cmp_func_by_pos f_list in
    let f_list = List.sort cmp_func_by_id f_list in
    let f func = 
        let pos = get_meta func in
        let id = fun_id' (unwrap func) in 
        let first = List.find (fun f -> fun_id' (unwrap f) = id) f_list in
        let lnum = let m = get_meta first in m.lnum in 
        let s = Printf.sprintf "Function %s already defined @ %d" id lnum in
        (pos, s)
    in
    let bad = Common.nonunique cmp_func_by_id f_list in 
    let erf = make_error_fun f in
    erf bad

let cmp_by_meta n1 n2 = 
    n1.m.lnum - n2.m.lnum

let arg_len func = 
    let func = unwrap func in
    let Fun'(_, Args(args), _) = func in
    let len_args = List.length args in
    len_args

(* exactly one "main" without params *)
let verify_main_func f_list = 
    let mains = List.filter (fun x -> fun_id' (unwrap x) = "main") f_list in
    if List.length mains = 0 then
        let s = "There must be exactly one \"main\" function (with no params)" in
        let error = Error({lnum=0}, s) in
        [error]
    else
        (* we don't care about dupes, since verify_funs will take care of that *)
        let mains = List.sort cmp_by_meta mains in
        let bad = List.filter (fun x-> arg_len x > 0) mains in
        let f func =
            let pos = get_meta func in
            let s = Printf.sprintf "\"main\" can't have parameters (this one has %d)" (arg_len func) in
            (pos, s)
        in
        let erf = make_error_fun f in
        erf bad

let verify_prog p = 
    let Prog'(f_list) = p in
    let f_id_count = List.map (fun f -> (fun_id' (unwrap f), arg_len f)) f_list in 
    let f_ids = List.map fst f_id_count in 
    let f acc func = 
        let func = unwrap func in
        let err1 = verify_jumps_in_func func in
        let err2 = verify_vars_in_func func in
        let err3 = verify_calls f_ids func in
        let err4 = verify_calls_params f_id_count func in
        let err = err1 @ err2 @ err3 @ err4 in
        let err = sort_and_cut err in
        (err :: acc)
    in
    let errors = List.fold_left f [] f_list in
    let errors = List.concat errors in
    let errors = errors @ verify_funs f_list in 
    let errors = errors @ verify_main_func f_list in 
    let errors = sort_by_pos errors in
    errors

let dump_error error = 
    let Error(pos, s) = error in
    let lnum = pos.lnum in
    let s = Printf.sprintf "%d: %s" lnum s in
    s

let dump_errors errors = List.map dump_error errors

(* Flattening *)

(* exp. flattening
 * BinOp(e1, op, e2) ->
 * tmp1 = flat(e1)
 * tmp2 = flat(e2)
 * BinOp(tmp1,op,tmp2)
*)
let rec wrap_flatten_exp exp n =
    let new_tmp n = 
            let tmp = "tmp" ^ string_of_int n in
            let eid = Var(tmp) in
            eid, tmp
    in
    let wrap e l n = 
        match e with
        | Const x -> e,l,n
        | Var x -> e,l,n
        | Ref x -> e,l,n
        | _ -> 
            let e', tmp = new_tmp n in
            let assign = Assign(tmp, e) in
            e',assign::l,n+1
    in
    let e', l, n = flatten_exp exp n in
    let e', l, n = wrap e' l n in
    e', l, n
and
flatten_exp exp n =
    match exp with
    | Const x -> exp, [], n
    | Var x -> exp, [], n
    | Ref x -> exp, [], n
    | ReadMem(id) -> exp, [], n
    | UnOp(op,e) -> 
        let e', l, n = wrap_flatten_exp e n in
        UnOp(op,e'), l, n
        
    | BinOp(e1, op, e2) ->
        let e1', l1, n = wrap_flatten_exp e1 n in
        let e2', l2, n = wrap_flatten_exp e2 n in
        let l = l1 @ l2 in
        BinOp(e1',op, e2'), l, n

let flatten_stmt s n = 
    let handle_call id el n =
        let f (ids, ll, n) e =
            let v,l',n = wrap_flatten_exp e n in
            (v::ids, l'::ll, n)
        in
        let (ids, ll, n) = List.fold_left f ([], [], n) el in
        let l = List.concat ll in
        let ids = List.rev ids in 
        ids, l, n
    in
    match s with 
    | DerefAssign(id, e) -> 
        let e', l, n = flatten_exp e n in
        DerefAssign(id,e') :: l, n
    | Assign(id, e) -> 
        let e', l, n = flatten_exp e n in
        Assign(id,e') :: l, n
    | WriteMem(id, e) ->
        let e', l, n = flatten_exp e n in
        WriteMem(id,e') :: l, n
    | Cmp(e1, e2) ->
        let v1, l1, n = wrap_flatten_exp e1 n in
        let v2, l2, n = wrap_flatten_exp e2 n in
        let l = l1 @ l2 in
        Cmp(v1, v2) :: l, n
    | Call(id, ExpArgs(el)) -> 
        let ids, l, n = handle_call id el n in
        let c = Call(id, ExpArgs(ids)) in
        c::l, n
    | ExtCall(id, ExpArgs(el)) ->
        let ids, l, n = handle_call id el n in
        let c = ExtCall(id, ExpArgs(ids)) in
        c::l, n
    | _ -> [s], n

let flatten_fun_body fb = 
    let f (ll, n) stmt =
        let l, n = flatten_stmt stmt n in
        (l::ll, n)
    in
    let stmts = match fb with FunBody(stmts) -> stmts in
    let (ll, n) = List.fold_left f ([], 0) stmts in
    let l = List.concat ll in
    let l = List.rev l in 
    l

let flatten_fun func = 
    match func with
    | Fun(id, args, body) ->
        let l = flatten_fun_body body in
        let fb = FunBody(l) in
        Fun(id, args, fb)

let flatten_prog p = 
    match p with 
    | Prog(func_list) -> 
        let fl = List.map flatten_fun func_list in
        Prog(fl)

let move_main_to_front p = 
    let rec aux main acc l = 
        match l with
        | hd::tl -> 
            let Fun(id,_,_) = hd in
            if id="main" then
                aux (Some(hd)) acc tl
            else
                aux main (hd::acc) tl
        | _ -> 
            begin
                match main with
                | Some(f) -> f::(List.rev acc)
                | None -> assert false
            end
    in
    let Prog(func_list) = p in
    let func_list = aux None [] func_list in
    Prog(func_list)
