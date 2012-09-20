(** Utility functions for ASTs.  It's useful to have these in a
    separate file so it can use functions from Typecheck and elsewhere. *)

open Ast
open Big_int
open Big_int_convenience
open Type
open Typecheck


exception RangeNotFound of int64 * int64

(* exp helpers *)
let binop op a b = match (a,b) with
  | (Int(a, at), Int(b, bt)) when at = bt ->
    let (i,t) = Arithmetic.binop op (a,at) (b,bt) in
    Int(i,t)
  | _ -> BinOp(op, a, b)

let unop op a = match a with
  | Int(a, at) ->
      let (i,t) = Arithmetic.unop op (a,at) in
      Int(i,t)
  | _ -> UnOp(op, a)

let ( +* ) a b   = binop PLUS a b
let ( -* ) a b   = binop MINUS a b
let ( ** ) a b   = binop TIMES a b
let ( <<* ) a b  = binop LSHIFT a b
let ( >>* ) a b  = binop RSHIFT a b
let ( >>>* ) a b = binop ARSHIFT a b
let ( &* ) a b   = binop AND a b
let ( |* ) a b   = binop OR a b
let ( ^* ) a b   = binop XOR a b
let ( ==* ) a b  = binop EQ a b
let ( <>* ) a b  = binop NEQ a b
let ( <* ) a b   = binop LT a b
let ( >* ) a b   = binop LT b a
(** bitwise equality *)
let ( =* ) a b   = binop XOR a (unop NOT b)

let cast_low t e = Cast(CAST_LOW, t, e)
let cast_high t e = Cast(CAST_HIGH, t, e)
let cast_signed t e = Cast(CAST_SIGNED, t, e)
let cast_unsigned t = function
  | Cast(CAST_UNSIGNED, Reg t', e) when Arithmetic.bits_of_width t >= t' ->
    Cast(CAST_UNSIGNED, t, e)
  | e ->
    Cast(CAST_UNSIGNED, t, e)

let exp_ite ?t b e1 e2 =
  (* FIXME: were we going to add a native if-then-else thing? *)
  (* type inference shouldn't be needed when t is specified, but we're paranoid *)
  let tb = Typecheck.infer_ast ~check:false b in
  let t1 = Typecheck.infer_ast ~check:false e1 in
  let t2 = Typecheck.infer_ast ~check:false e2 in
  assert (t1 = t2);
  assert (tb = Reg(1));

  (match t with
    | None -> ()
    | Some t -> assert (t=t1));

  Ite(b, e1, e2)


let parse_ite = function
  | BinOp(OR,
	  BinOp(AND, Cast(CAST_SIGNED, _, b1), e1),
	  BinOp(AND, Cast(CAST_SIGNED, _, UnOp(NOT, b2)), e2)
  )
  | BinOp(OR,
	  BinOp(AND, b1, e1),
	  BinOp(AND, UnOp(NOT, b2), e2)
  ) when full_exp_eq b1 b2 && Typecheck.infer_ast ~check:false b1 = Reg(1) ->
    Some(b1, e1, e2)
      (* In case one branch is optimized away *)
  | BinOp(AND,
	  Cast(CAST_SIGNED, nt, b1),
	  e1) when Typecheck.infer_ast ~check:false b1 = Reg(1) ->
    Some(b1, e1, Int(zero_big_int, nt))
  | _ -> None

let parse_extract = function
     | Cast(CAST_LOW, t, BinOp(RSHIFT, e', Int(i, t2))) ->
     	 (*
     	    Original: extract 0:bits(t)-1, and then shift left by i bits.
     	    New: extract i:bits(t)-1+i
     	 *)
     	 let et = infer_ast ~check:false e' in
     	 let bits_t = big_int_of_int (bits_of_width t) in
     	 let lbit = i in
     	 let hbit = (lbit +% bits_t) -% bi1 in
     	 (* XXX: This should be unsigned >, but I don't think it matters. *)
     	 if hbit >% big_int_of_int(bits_of_width et) then
     	   None
	 else
	   Some(hbit, lbit)
     | _ -> None

let parse_concat = function
    (* Note: We should only parse when we would preserve the type.
       So, (nt1=nt2) = bits(er) + bits(el)

       XXX: When we convert to normalized memory access, we get
       expressions like Cast(r32)(mem[0]) @ Cast(r32)(mem[1]) << 8 @
       ....  It sure would be nice if we could recognize this as a
       series of concats. *)
  | BinOp(OR,
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)),
	  Cast(CAST_UNSIGNED, nt2, er))
  | BinOp(OR,
	  Cast(CAST_UNSIGNED, nt2, er),
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)))
      when nt1 = nt2
	&& bits ==% big_int_of_int(bits_of_width (infer_ast ~check:false er))
	&& bits_of_width nt1 = bits_of_width (infer_ast ~check:false el) + bits_of_width (infer_ast ~check:false er) (* Preserve the type *)
	->
      Some(el, er)
  | BinOp(OR,
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)),
	  (Int(i, nt2) as er))
  | BinOp(OR,
	  (Int(i, nt2) as er),
	  BinOp(LSHIFT,
		Cast(CAST_UNSIGNED, nt1, el),
		Int(bits, _)))
      (* If we cast to nt1 and nt2 and we get the same thing, the
	 optimizer probably just dropped the cast. *)
      when Arithmetic.to_big_int (i, nt2) ==% Arithmetic.to_big_int (i, nt1)
	&& bits ==% big_int_of_int(bits_of_width (infer_ast ~check:false er))
	&& bits_of_width nt1 = bits_of_width (infer_ast ~check:false el) + bits_of_width (infer_ast ~check:false er) (* Preserve the type *)
	->
      Some(el, er)
  | _ -> None

(* Functions for removing expression types

   Should these recurse on subexpressions?
*)
let rm_ite = function
  | Ite(b, e1, e2) ->
      let t = Typecheck.infer_ast b in
      (match t with
      | Reg(1) ->
	(b &* e1) |*  (exp_not b &* e2)
      | Reg n ->
	((cast_signed t b) &* e1) |* ((cast_signed t (exp_not b)) &* e2)
      | _ -> failwith "rm_ite does not work with memories")
  | _ -> assert false (* Should we just act as a noop? *)

let rm_extract = function
  | Extract(h, l, e) ->
      let nb = int_of_big_int ((h -% l) +% bi1) in
      let nt = Reg(nb) in
      assert(h >=% bi0);
      assert (nb >= 0);
      let t = infer_ast ~check:false e in
      let e = if l <>% bi0 then e >>* Int(l, t) else e in
      let e = if t <> nt then cast_low nt e else e in
      e
  | _ -> assert false

let rm_concat = function
  | Concat(le, re) ->
      let bitsl,bitsr =
	Typecheck.bits_of_width (Typecheck.infer_ast ~check:false le),
	Typecheck.bits_of_width (Typecheck.infer_ast ~check:false re)
      in
      let nt = Reg(bitsl + bitsr) in
      exp_or ((cast_unsigned nt le) <<* Int(big_int_of_int bitsr, nt)) (cast_unsigned nt re)
  | _ -> assert false


(* Return list of statments between start_addr and end_addr *)
let find_prog_chunk prog start_addr end_addr = 
  let rec find_prog_chunk_k prog starta enda k =
	match prog with
	| [] -> raise (RangeNotFound (start_addr, end_addr))
	| p::ps -> 
	  match starta with
	  | Some(a) -> (match p with
		| Label(Addr(addr),attrs) -> 
		  (* If this is the start address we are looking for begin recording 
			 with accumulator k.  Set starta to None so that we know we are in
			 the desired range *)
		  if (addr = a) then find_prog_chunk_k ps None enda (p::k)
		  else find_prog_chunk_k ps starta enda k
		| _ -> find_prog_chunk_k ps starta enda k
	  )
	  (* Indicates we are inside desired block; return through end_addr *)
	  | None -> (match p with
		| Label(Addr(addr),attrs) -> 
		  if (addr = enda) then k
		  else find_prog_chunk_k ps starta enda (p::k)
		| _ -> find_prog_chunk_k ps starta enda (p::k)
	  )
  in
  List.rev (find_prog_chunk_k prog (Some start_addr) end_addr [])
