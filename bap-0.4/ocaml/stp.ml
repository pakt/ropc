(** Output to STP format (same as CVCL or CVC3)
*)

open Ast
open Big_int
open Big_int_convenience
open Type
open Typecheck

module D = Debug.Make(struct let name = "STP" and default=`Debug end)
open D



module VH = Var.VarHash




class pp ?suffix:(s="") ft =
  let pp = Format.pp_print_string ft
  and pc = Format.pp_print_char ft
  and pi = Format.pp_print_int ft
  and space = Format.pp_print_space ft
  and cut = Format.pp_print_cut ft
  and force_newline = Format.pp_force_newline ft
  and printf f = Format.fprintf ft f
  and opn  = Format.pp_open_box ft
  and flush = Format.pp_print_flush ft
  and cls = Format.pp_close_box ft in
  let var2s (Var.V(num,name,_)) =
    name^"_"^(string_of_int num)^s
  in

object (self)
  inherit Formulap.fpp
  val used_vars : (string,Var.t) Hashtbl.t = Hashtbl.create 57
  val ctx : string VH.t = VH.create 57
    
  val mutable unknown_counter = 0;

  val mutable let_counter = 0;

  method flush =
    flush();

  method extend v s =
    assert(not(Hashtbl.mem used_vars s));
    Hashtbl.add used_vars s v;
    VH.add ctx v s

  method unextend v =
    VH.remove ctx v

  method var v =
    try pp (VH.find ctx v)
    with Not_found ->
      let s = var2s v in
      self#extend v s; (* FIXME: is this really what we want? *)
      pp s

  method varname v =
    VH.find ctx v

  method declare_new_freevars e =
    opn 0;
    pp "% free variables:"; force_newline();
    let fvs = Formulap.freevars e in 
    List.iter (fun v -> if not(VH.mem ctx v) then self#decl v) fvs;
    pp "% end free variables."; force_newline();
    cls()
       
  method typ = function
    | Reg n ->	printf "BITVECTOR(%u)" n
    | Array(idx,elmt) -> pp "ARRAY "; self#typ idx; pp " OF "; self#typ elmt
    | TMem _ ->	failwith "TMem unsupported by STP"


  method decl (Var.V(_,_,t) as v) =
    self#extend v (var2s v);
    self#var v; pp " : "; self#typ t; pp ";"; force_newline();



  method ast_exp e =
    opn 0;
    (match e with
     | Int(i,t) ->
	 let maskedval = Arithmetic.to_big_int (i,t) in
	 (match t with
	   | Reg n when (n mod 4) = 0 ->
	       printf "0hex%s" (Util.hex_of_big_int ~pad:(n/4) maskedval)
	   | Reg n ->
	       printf "0bin%s" (Util.binary_of_big_int ~pad:n maskedval)
	   | _ -> invalid_arg "Only constant integers supported")
     | Ite(b, v1, v2) ->
	 (* XXX: Needs testing *)
	 pp "(IF";
	 space ();
	 pc '(';
	 self#ast_exp b;
	 pp "=0bin1";
	 (* Do we need to add = 0bin1? Yes. *)
	 pc ')';
	 space ();
	 pp "THEN";
	 space ();
	 self#ast_exp v1;
	 space ();
	 pp "ELSE";
	 space ();
	 self#ast_exp v2;
	 space ();
	 pp "ENDIF)"
     | Extract(h,l,e) ->
	 pp "(";
	 self#ast_exp e;
	 pp ")[";
	 pi (int_of_big_int h);
	 pc ':';
	 pi (int_of_big_int l);
	 pc ']'
     | Concat(le,re) ->
	 pc '(';
	 self#ast_exp le;
	 pc '@';
	 self#ast_exp re;
	 pc ')'
     | Var v ->
	 self#var v
     | UnOp(uop, o) ->
	 (match uop with
	  | NEG -> pp "BVUMINUS("
	  | NOT -> pp "~("
	 );
	 self#ast_exp o;
	 pc ')'
	   (* Eww, the << operator in stp wants a constant int on the right,
	      rather than a bitvector *)
     | BinOp((LSHIFT|RSHIFT|ARSHIFT), e1, Int(i,_)) when bi_is_zero i ->
	 (* STP barfs on 0, so we don't put the shift *)
	 self#ast_exp e1
     | BinOp(LSHIFT, e1, Int(i,_)) ->
	 let  t = infer_ast ~check:false e1 in
	 pp "(("; self#ast_exp e1; pp" << "; pp (string_of_big_int i); pp ")[";
	 pp (string_of_int(bits_of_width t - 1)); pp":0])"
     | BinOp(RSHIFT, e1, Int(i,_)) -> (* Same sort of deal :( *)
	 pc '('; self#ast_exp e1; pp " >> "; pp(string_of_big_int i); pc ')'
     | BinOp(ARSHIFT, e1, Int(i,_)) -> (* Same sort of deal :( *)
	 let t = infer_ast ~check:false e1 in
	 let bits = string_of_int (bits_of_width t) in
	 let gethigh = sub_big_int (big_int_of_int (bits_of_width t)) i in
	 let gethigh = sub_big_int gethigh bi1 in
	 if ge_big_int gethigh bi0 then (
	   pp "SX(("; self#ast_exp e1;
	   pp " >> "; pp (string_of_big_int i);
	   pp ")["; pp (string_of_big_int gethigh); pp ":0], "; pp bits; pc ')'
	 ) else (
	   let b = Int64.sub (Int64.of_int (bits_of_width t)) 1L in
	   pp "SX("; self#ast_exp e1; pp "["; pp (Int64.to_string b);
	   pp ":"; pp (Int64.to_string b); pp "], "; pp bits; pc ')';
	 )
      | BinOp((LSHIFT|RSHIFT|ARSHIFT) as bop, e1, e2) ->
	  let t2 = infer_ast ~check:false e2 in
	  let const n = Int(biconst n,t2) in
	  let put_one n = self#ast_exp (BinOp(bop, e1, const n)) in
	  let rec put_all n =
	    if n < 64 then (
	      pp " IF ";
	      self#ast_exp e2;
	      pp " = ";
	      self#ast_exp (const n);
	      pp " THEN ";
	      put_one n;
	      pp " ELSE ";
	      put_all (n+1);
	      pp " ENDIF "
	    ) else put_one n
	  in
	  put_all 0;
      | BinOp(bop, e1, e2) as e ->
	  let t = infer_ast ~check:false e1 in
	  let t' = infer_ast ~check:false e2 in
	  if t <> t' then
	    wprintf "Type mismatch: %s" (Pp.ast_exp_to_string e);
	  assert (t = t') ;
	  let bits = if is_integer_type t then  bits_of_width t else -1 in
	  let sw = string_of_int bits in
	  let (pre,mid,post) = match bop with
	    | PLUS     -> ("BVPLUS("^sw^", ", ",", ")")
	    | MINUS    -> ("BVSUB("^sw^", ", ",", ")")
	    | TIMES    -> ("BVMULT("^sw^", ", ",", ")")
	    | DIVIDE   -> ("BVDIV("^sw^", ", ",", ")")
	    | SDIVIDE  -> ("SBVDIV("^sw^", ", ",", ")")
	    | MOD      -> ("BVMOD("^sw^", ", ",", ")")
	    | SMOD     -> ("SBVMOD("^sw^", ", ",", ")")
	    | AND      -> ("(", "&", ")")
	    | OR       -> ("(", "|", ")")
	    | XOR      -> ("BVXOR(", ",", ")")
	    | EQ       -> ("IF (", "=", ") THEN 0bin1 ELSE 0bin0 ENDIF")
	    | NEQ      -> ("IF (NOT(", "=", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LT       -> ("IF (BVLT(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LE       -> ("IF (BVLE(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | SLT      -> ("IF (BVSLT(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | SLE      -> ("IF (BVSLE(", ",", ")) THEN 0bin1 ELSE 0bin0 ENDIF")
	    | LSHIFT 
	    | ARSHIFT
	    | RSHIFT ->
		failwith "shifts should have been handled by a different case"
	  in
	  pp pre;
	  self#ast_exp e1;
	  pp mid;
	  cut();
	  self#ast_exp e2;
	  pp post
      | Cast(ct,t, e1) ->
	  let t1 = infer_ast ~check:false e1 in
	  let (bits, bits1) = (bits_of_width t, bits_of_width t1) in
	  let (pre,post) = match ct with
	    | CAST_SIGNED    -> ("SX(",", "^string_of_int bits^")")
	    | CAST_LOW       -> ("", "["^string_of_int(bits - 1)^":0]")
	    | CAST_HIGH      ->
		("", "["^string_of_int(bits1-1)^":"^string_of_int(bits1-bits)^"]")
	    | CAST_UNSIGNED  ->
		if bits = bits1 then ("","") else
		  ("(0bin"^String.make (bits-bits1) '0'^" @ ", ")")
		  (* @ does not work right in CVC3, so using BVPLUS... I think they fixed it now -ed *)
		  (* ("(BVPLUS(" ^ string_of_int bits ^ ",", ",0bin0))") *)
	  in
	  pp pre;
	  self#ast_exp e1;
	  pp post
      | Unknown(s,t) ->
	  pp "unknown_"; pi unknown_counter; pp" %"; pp s; force_newline();
	  unknown_counter <- unknown_counter + 1;
      | Lab lab ->
	  failwith ("STP: don't know how to handle label names: "
		      ^ (Pp.ast_exp_to_string e))
      | Let(v, e1, e2) ->
	  pp "(LET ";
	  (* v isn't allowed to shadow anything *)
	  let s = var2s v ^"_"^ string_of_int let_counter in
	  let_counter <- succ let_counter;
	  pp s;
	  pp " =";
	  opn 2; space();
	  self#ast_exp e1;
	  space(); cls();
	  pp "IN"; space();
	  self#extend v s;
	  self#ast_exp e2;
	  self#unextend v;
	  pc ')'
      | Load(arr,idx,endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  self#ast_exp arr;
	  pc '[';
	  self#ast_exp idx;
	  pc ']'
      | Store(arr,idx,vl, endian, t) ->
	  (* FIXME check arr is array and not mem *)
	  pc '(';
	  self#ast_exp arr;
	  pp " WITH [";
	  self#ast_exp idx;
	  pp "] := ";
	  self#ast_exp vl;
	  pc ')'
    );
    cls();



  method forall = function
    | [] -> ()
    | v::vars ->
	let var_type  (Var.V(_,_,t) as v) =
	  self#var v; pp " : "; self#typ t
	in
	opn 2;
	pp "FORALL (";space();
	  (* TODO: group by type *)
	List.iter (fun v -> var_type v; pc ','; space()) vars;
	var_type v;
	pp "):";
	cls();space();

  method exists = function
    | [] -> ()
    | v::vars ->
	let var_type  (Var.V(_,_,t) as v) =
	  self#var v; pp " : "; self#typ t
	in
	opn 2;
	pp "EXISTS (";space();
	  (* TODO: group by type *)
	List.iter (fun v -> var_type v; pc ','; space()) vars;
	var_type v;
	pp "):";
	cls();space();

  method assert_eq v e =
    opn 0;
    self#declare_new_freevars (BinOp(EQ, Var v, e));
    force_newline();
    pp "ASSERT(";
    self#var v;
    pc '=';
    self#ast_exp e;
    pp ");";
    cls()

  method assert_ast_exp_with_foralls ?(fvars=true) foralls e =
    opn 0;
    if fvars then (
      self#declare_new_freevars e;
      force_newline();
    );
    pp "ASSERT(";
    space();
    self#forall foralls;
    pp "0bin1 =";
    force_newline();
    self#ast_exp e;
    force_newline();
    pp ");";
    force_newline();
    pp "QUERY(FALSE);";
    cls();

  (** Is e a valid expression (always true)? *)
  method valid_ast_exp ?(exists=[]) ?(foralls=[]) e =
    opn 0;
    self#declare_new_freevars e;
    force_newline();
    pp "QUERY(";
    space();    
    self#exists exists;
    self#forall foralls;
    pp "0bin1 =";
    force_newline();
    self#ast_exp e;
    force_newline();
    pp ");";
    cls();    

  method assert_ast_exp e =
    self#assert_ast_exp_with_foralls [] e

  method counterexample =
    force_newline();
    pp "COUNTEREXAMPLE;";
    cls()

  method close =
    Format.pp_print_newline ft ();

end


class pp_oc ?suffix:(s="") fd =
  let ft = Format.formatter_of_out_channel fd in
object
  inherit pp ~suffix:s ft as super
  inherit Formulap.fpp_oc
  method close =
    super#close;
    close_out fd
end

