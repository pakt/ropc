(** Pretty printing

    @todo Write .mli
*)

open Big_int
open Big_int_convenience
open Type
module VH = Var.VarHash
module F = Format

module D = Debug.Make(struct let name = "pp" and default=`Debug end)
open D

let output_varnums = ref false

let many_parens = ref false

let rec typ_to_string = function
  | Reg 1 -> "bool"
  | Reg 8 -> "u8"
  | Reg 16 -> "u16"
  | Reg 32 -> "u32"
  | Reg 64 -> "u64"
  | Reg n -> Printf.sprintf "u%u" n
  | TMem t -> "?" ^ typ_to_string t
  | Array(idx,e) -> typ_to_string e ^ "?" ^ typ_to_string idx


let ct_to_string = function
  | CAST_UNSIGNED  -> "pad"
  | CAST_SIGNED -> "extend"
  | CAST_HIGH -> "high"
  | CAST_LOW -> "low"

let binop_to_string = function
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | SDIVIDE -> "$/"
  | MOD -> "%"
  | SMOD -> "$%"
  | LSHIFT -> "<<"
  | RSHIFT -> ">>"
  | ARSHIFT -> "$>>"
  | AND -> "&"
  | OR -> "|"
  | XOR -> "^"
  | EQ -> "=="
  | NEQ -> "<>"
  | LT -> "<"
  | LE -> "<="
  | SLT -> "$<"
  | SLE -> "$<="

let unop_to_string = function
  | NEG -> "-"
  | NOT -> "~"



type varctx = string VH.t * (string,unit) Hashtbl.t

let var_to_string ?ctx (Var.V(id,name,t) as v) =
  match ctx with
  | None ->
	name ^ "_" ^ string_of_int id ^ ":" ^ typ_to_string t
  | Some(vars,names) ->
      try VH.find vars v
      with Not_found ->
	let rec trystring (s, `F next) =
	  if Hashtbl.mem names s then
	    trystring (next s)
	  else (
	    let s' = s ^":"^ typ_to_string t in
	    VH.add vars v s';
	    Hashtbl.add names s ();
	    s')
	in
	let rec more x = (x^"_", `F more) in
	trystring (name, `F (fun _ -> (name ^ "_" ^ string_of_int id, `F more)))

class pp ft =
  let pp = F.pp_print_string ft
  and pc = F.pp_print_char ft
  and space = Format.pp_print_space ft
  and printf = Format.fprintf ft
  and opn  = F.pp_open_box ft
  and cls = F.pp_close_box ft in
  let comma () = pp ","; space() in
  let vctx = (VH.create 100, Hashtbl.create 100) in
object (self)


  method var v = 
    if !output_varnums then
      pp (var_to_string v)
    else
      pp (var_to_string ~ctx:vctx v)

  method typ t = pp (typ_to_string t)


  method attrs a = List.iter (fun a -> space();self#attr a) a

  method attr = function
    | Asm s -> pp "@asm \""; pp s; pp "\""
    | Address a -> printf "@address \"0x%Lx\"" a;
    | Liveout -> pp "@set \"liveout\""
    | StrAttr s -> pp "@str \""; pp s; pc '\"'
    | Context {name=s; mem=mem; value=v; index=i; t=Reg bits; taint=Taint t} -> 
	let ts = string_of_int t in
	(*if t = Taint then "tainted" else "untainted" in*)
	let ind = if mem then "[0x"^(Int64.format "%Lx" i)^"]" else "" in
	pp "@context "; pp (s^ ind ^" = 0x"^(Util.hex_of_big_int v)^ ", " ^ ts
			      ^", u"
			      ^ (string_of_int bits))
    | Context _ ->
      failwith "Contexts only specify register types"
    | ThreadId i -> pp "@tid \""; pp (string_of_int i); pp "\""
    | ExnAttr _ -> () (* we could try to print something using Printexc.to_string *)
    | Pos (_,addr) -> pp ("@pos"^(string_of_int addr)) (* ignore position attrs *)
    | InitRO -> pp "@set \"initro\""
    | Synthetic -> pp "@set \"synthetic\""

  method label = function
    | Name s -> pp "label "; pp s
    | Addr x -> printf "addr 0x%Lx" x

  method int i t =
    let (is, i) = Arithmetic.to_sbig_int (i,t), Arithmetic.to_big_int (i,t) in
    match (is, t) with
    | (bi, Reg 1) when bi_is_zero bi -> pp "false"
    | (bi, Reg 1) when bi_is_minusone bi -> pp "true"
    | (bi,t) ->
        if (abs_big_int bi) <% bia
	then pp (string_of_big_int bi)
        else pp ("0x"^(Util.hex_of_big_int (Arithmetic.to_big_int (i,t))));
	pp ":"; self#typ t


  (* prec tells us how much parenthization we need. 0 means it doesn't need
     to be parenthesized. Larger numbers means it has higher precedence.
     Maximum prec before paretheses are added are as follows:
     5 Let
     7 Ite
     10 Store
     12 Concat
     15 Extract
     20 OR
     30 XOR
     40 AND
     50 EQ NEQ
     60 LT SLT SLE LE
     70 LSHIFT RSHIFT ARSHIFT
     80 PLUS MINUS
     90 TIMES DIVIDE SDIVIDE MOD SMOD
     100 UMINUS NOT
     110 Get
     Because we don't distinguish precedence to the right or left, we will
     always overparethesise expressions such as:
     let x = y in x + let x = 2:reg32_t in x
  *)
  method ast_exp ?(prec=0) e =
    let lparen bind = if !many_parens || bind < prec then pp "("
    and rparen bind = if !many_parens || bind < prec then pp ")"
    and binop_prec = function
      | OR -> 20
      | XOR -> 30
      | AND -> 40
      | EQ | NEQ -> 50
      | LT | SLT | SLE | LE -> 60
      | LSHIFT | RSHIFT | ARSHIFT -> 70
      | PLUS | MINUS -> 80
      | TIMES | DIVIDE | SDIVIDE | MOD | SMOD -> 90
    in
    opn 0;
    (match e with
     | Ast.Load(arr,idx,edn,t) ->
	 lparen 110;
	 self#ast_exp ~prec:110 arr;
	 pp "["; self#ast_exp idx; comma(); self#ast_endian edn; pp "]";
	 (* FIXME: check type of arr *)
	 pp ":"; self#typ t;
	 rparen 110
     | Ast.Store(arr,idx,vl, edn, t) ->
	 lparen 10;
	 self#ast_exp ~prec:10 arr;
	 pp " with"; space();
	 pp "["; self#ast_exp idx;
	 comma(); self#ast_endian edn;
	 pp "]:"; self#typ t;
	 pp " ="; space();
	 self#ast_exp ~prec:10 vl;
	 rparen 10;
     | Ast.Ite(c,x,y) ->
	 lparen 7;
	 pp "if";
	 space ();
	 self#ast_exp ~prec:7 c;
	 space ();
	 pp "then";
	 space ();
	 self#ast_exp ~prec:7 x;
	 space ();
	 pp "else";
	 space ();
	 self#ast_exp ~prec:7 y;	 
	 rparen 7
     | Ast.Extract(h, l, e) ->
	 pp "extract:";
	 pp (string_of_big_int h);
	 pc ':';
	 pp (string_of_big_int l);
	 pc ':';
	 pc '[';
	 self#ast_exp e;
	 pc ']';
     | Ast.Concat(le, re) ->
	 pp "concat:";
	 pc '[';
	 self#ast_exp le;
	 pp "][";
	 self#ast_exp re;
	 pc ']'
     | Ast.BinOp(b,x,y) ->
	 let p = binop_prec b in
	 lparen p;
	 self#ast_exp ~prec:p x;
	 pp " "; pp (binop_to_string b); space();
	 self#ast_exp ~prec:(p+1) y;
	 rparen p
     | Ast.UnOp(u, x) ->
	 lparen 100;
	 pp (unop_to_string u); self#ast_exp ~prec:100 x;
	 rparen 100
     | Ast.Var v ->
	 self#var v
     | Ast.Lab s ->
	 (* FIXME: quote s? *)
	 pp "\""; pp s; pp "\"";
     | Ast.Int(i,t) ->
         self#int i t
     | Ast.Cast(ct,t,e) ->
	 pp (ct_to_string ct);
	 pp ":"; self#typ t;
	 pp "("; self#ast_exp e; pp ")"
     | Ast.Let(v,e1,e2) ->
	 lparen 5;
	 pp "let "; self#var v; pp " :=";
	 opn 2; space();
	 self#ast_exp ~prec:5 e1; space(); 
	 cls();
	 pp "in"; space();
	 self#ast_exp ~prec:5 e2;
	 rparen 5
     | Ast.Unknown(s,t) ->
	 pp "unknown \""; pp s; pp "\":"; self#typ t
    );
    cls();

  method ast_endian = function
    | Ast.Int(bi, Reg 1) when bi_is_zero bi ->
	pp "e_little";
    | Ast.Int(bi, Reg 1) when bi_is_one bi ->
	pp "e_big"
    | x -> self#ast_exp x

  method ast_stmt s =
    opn 2;
    (match s with
    | Ast.Move(v, e, a) ->
	self#var v;
	pp " ="; space();
	self#ast_exp e;
	self#attrs a
    | Ast.Jmp(e,a) ->
	pp "jmp ";
	self#ast_exp e;
	self#attrs a;
    | Ast.CJmp(c,t,f,a) ->
	pp "cjmp"; space();
	self#ast_exp c; comma();
	self#ast_exp t; comma();
	self#ast_exp f;
	self#attrs a
    | Ast.Label(l,a) ->
	self#label l;
	self#attrs a
    | Ast.Halt(e,a) ->
	pp "halt ";
	self#ast_exp e;
	self#attrs a
    | Ast.Assert(e,a) ->
	pp "assert ";
	self#ast_exp e;
	self#attrs a
    | Ast.Comment(s,a) ->
	pp "/*";
	pp s;
	pp "*/";
	self#attrs a
    | Ast.Special(s,a) ->
	pp "special \"";
	pp s;
	pp "\"";
	self#attrs a);
    cls();

    (* addr, mnemonic *)
  method ast_stmt_short s =
    (match s with
    | Ast.Label(Addr(x) as l,a) ->
	self#label l;
    self#attrs a
    | _ -> ()
	);

  method ast_program p =
    Format.pp_open_hvbox ft 0;
    List.iter (fun x -> self#ast_stmt x; space()) p;
    Format.pp_force_newline ft ();
    cls();


  method ssa_value = function
    | Ssa.Int(i,t) ->
        self#int i t
    | Ssa.Var v ->
	self#var v
    | Ssa.Lab lab ->
	pc '"'; pp lab; pc '"'

  method ssa_endian = function
    | Ssa.Int(bi, Reg 1) when bi_is_zero bi -> pp "e_little";
    | Ssa.Int(bi, Reg 1) when bi_is_one bi -> pp "e_big"
    | x -> self#ssa_value x

  method ssa_exp e =
    opn 0;
    (match e with
     | Ssa.Load(arr,idx,edn, t) ->
	 self#ssa_value arr;
	 pp "["; self#ssa_value idx; comma(); self#ssa_endian edn; pp "]";
	 (* FIXME: check type of arr *)
	 pp ":"; self#typ t;
     | Ssa.Store(arr,idx,vl, edn, t) ->
	 self#ssa_value arr;
	 pp " with"; space();
	 pp "["; self#ssa_value idx;
	 comma(); self#ssa_endian edn;
	 pp "]:"; self#typ t;
	 pp " ="; space();
	 self#ssa_value vl
     | Ssa.Ite(c, x, y) ->
	 pp "if";
	 space ();
	 self#ssa_value c;
	 space ();
	 pp "then";
	 space ();
	 self#ssa_value x;
	 space ();
	 pp "else";
	 space ();
	 self#ssa_value y	 
     | Ssa.Extract(h, l, e) ->
	 pp "extract:";
	 pp (string_of_big_int h);
	 pc ':';
	 pp (string_of_big_int l);
	 pp ":[";
	 self#ssa_value e;
	 pc ']';
     | Ssa.Concat(lv, rv) ->
	 pp "concat:[";
	 self#ssa_value lv;
	 pp "][";
	 self#ssa_value rv;
	 pc ']'
     | Ssa.BinOp(b, x, y) ->
	 self#ssa_value x;
	 pp " "; pp (binop_to_string b); space();
	 self#ssa_value y;
     | Ssa.UnOp(u, x) ->
	 pp (unop_to_string u); self#ssa_value x;
     | Ssa.Val v ->
	 self#ssa_value v
     | Ssa.Cast(ct,t,v) ->
	 pp (ct_to_string ct);
	 pp ":"; self#typ t;
	 pp "("; self#ssa_value v; pp ")"
     | Ssa.Unknown(s,t) ->
	 pp "unknown \""; pp s; pp "\":"; self#typ t
     | Ssa.Phi [] ->
	 pp "(ERROR: Empty phi)"
     | Ssa.Phi(x::xs) ->
	 pp "phi(";
	 self#var x;
	 List.iter (fun x -> pp ", "; self#var x) xs;
	 pp ")"
    );
    cls();

  method ssa_stmt s =
    opn 2;
    (match s with
    | Ssa.Move(v,e,a) ->
	self#var v;
	pp " ="; space();
	self#ssa_exp e;
	self#attrs a
    | Ssa.Jmp(v,a) ->
	pp "jmp ";
	self#ssa_value v;
	self#attrs a
    | Ssa.CJmp(c,t,f,a) ->
	pp "cjmp"; space();
	self#ssa_value c; comma();
	self#ssa_value t; comma();
	self#ssa_value f;
	self#attrs a
    | Ssa.Label(l,a) ->
	(match l with
	 | Name s -> pp "label "; pp s
	 | Addr x -> printf "addr 0x%Lx" x
	);
	self#attrs a
    | Ssa.Halt(v,a) ->
	pp "halt ";
	self#ssa_value v;
	self#attrs a
    | Ssa.Assert(v,a) ->
	pp "assert ";
	self#ssa_value v;
	self#attrs a
    | Ssa.Comment(s,a) ->
	pp "/*";
	pp s;
	pp "*/";
	self#attrs a
    );
    cls()

  method ssa_stmts stmts =
    Format.pp_open_hvbox ft 0;
    List.iter (fun s -> self#ssa_stmt s; space()) stmts;
    cls()

  method close =
    Format.pp_print_newline ft ();
 
end

class pp_oc fd =
  let ft = Format.formatter_of_out_channel fd in
object
  inherit pp ft as super
  method close =
    super#close;
    if fd <> stdout then
      close_out fd
end



(* Argh, these should all be locals, but then ocaml's type system won't let
   pp2string be polymorphic *)
let buf = Buffer.create 57
let ft = 
  let out = Buffer.add_substring buf
  and spaces _ = Buffer.add_char buf ' ' in
  let ft = Format.formatter_of_buffer buf in
  Format.pp_set_all_formatter_output_functions ft ~out ~flush:ignore ~spaces ~newline:ignore;
  ft
let strpp = new pp ft

let pp2string f v =
     Format.pp_open_box ft 0;
     f strpp v;
     Format.pp_print_flush ft ();
     let s = Buffer.contents buf in
     Buffer.reset buf;
     s


let value_to_string = pp2string (fun p -> p#ssa_value)
let label_to_string = pp2string (fun p -> p#label)
let ssa_exp_to_string = pp2string (fun p -> p#ssa_exp)
let ssa_stmt_to_string = pp2string (fun p -> p#ssa_stmt)
let ast_exp_to_string = pp2string (fun p -> p#ast_exp ~prec:0)
let ast_stmt_to_string = pp2string (fun p -> p#ast_stmt)

let ast_short_dump stmts = 
    let p s = 
        match s with
        | Ast.Label(Addr _,_) -> true
        | _ -> false
    in
    let labels = List.filter p stmts in
    let dumper = pp2string (fun p -> p#ast_stmt_short) in
    let strings = List.map dumper labels in
    strings
