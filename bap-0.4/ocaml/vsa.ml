(** Value-Set Analysis / Value-Set Arithmetic

    See Gogul Balakrishnan's thesis at
    http://pages.cs.wisc.edu/~bgogul/Research/Thesis/thesis.html

*)

module VM = Var.VarMap

open Big_int
open Util
open Type
open Ssa

module D = Debug.Make(struct let name = "VSA" and default=`Debug end)
open D


exception Unimplemented of string
    
let rec uint64_gcd x y =
  if y = 0L then x
  else uint64_gcd y (int64_urem x y)

module I = Int64
(* some operators to make this more readable *)
let (&%) = I.logand
let (|%) = I.logor
let (^%) = I.logxor
let (+%) = I.add
let (-%) = I.sub
let bnot = I.lognot


let bits_of_width = Typecheck.bits_of_width

let fwd_transfer_stmt_to_block f g node latice =
  List.fold_left (fun l n -> f n l) latice (Cfg.SSA.get_stmts g node)

(* FIXME: find a better way to get the stack pointer *)
let sp = List.find (fun (Var.V(_,n,_)) -> n = "R_ESP") (Asmir.decls_for_arch Asmir.arch_i386)


(** Strided Intervals *)
module SI =
struct
(* FIXME: some of these functions can return a stride of 1 for a singleton *)

  (** unsigned stride * signed lower bound * signed upper bound *)
  type t = int64 * int64 * int64

  let to_string (s,lb,ub) = Printf.sprintf "%Lu[%Ld,%Ld]" s lb ub

  let highbit k =
    if k = 1 then 1L else I.shift_left 1L (k-1)

  let extend k i = 
    if k <> 64 then
      let k' = 64-k in I.shift_right(I.shift_left i k') k'
    else i

  let trunc k i = 
    if k <> 64 then
      let k' = 64-k in I.shift_right_logical(I.shift_left i k') k'
    else i

  let maxi k = highbit k -% 1L
  let mini k = extend k (highbit k)
  let top k = (1L, mini k, maxi k)

  let single x = (0L,x,x)
  let of_bap_int i t = single (extend (bits_of_width t) i)

  let zero = single 0L
  let one = single 1L
  let minus_one = single (-1L)


  let is_reduced k (s,lb,ub) =
    assert(k>0 && k<=64);
    (lb >= mini k && ub <= maxi k) &&
      if s = 0L then lb = ub
      else lb < ub && let r1 = I.rem lb s and r2 = I.rem ub s in r1 = r2 || r2 -% r1 = s

  let check_reduced k si =
    if not(is_reduced k si)
    then failwith(string_of_int k^"-bit Strided Interval "^to_string si^" not in reduced form")

  let check_reduced2 k si1 si2 =
    check_reduced k si1; check_reduced k si2


  let renorm k ((a,b,c) as si) =
    let si' = if b = c then (0L,b,b) else si in
      check_reduced k si';
      si'

  let renormbin f k x y = renorm k (f k x y)
  let renormun f k x = renorm k (f k x)


  (** Addition of strided intervals *)
  let add k ((s1,lb1,ub1) as a) ((s2,lb2,ub2) as b) =
    check_reduced2 k a b;
    let lb' = lb1 +% lb2 
    and ub' = ub1 +% ub2 in
    let u = lb1 &% lb2 &% bnot lb' &% bnot(ub1 &% ub2 &% bnot ub')
    and v = ((lb1 ^% lb2) |% bnot(lb1 ^% lb'))
      &% (bnot ub1 &% bnot ub2 &% ub')
    and highbit = highbit k
    in
      if (u |% v) &% highbit = highbit then
	top k
      else (uint64_gcd s1 s2, extend k lb', extend k ub')

  let add = renormbin add

  (** Negation of a strided interval *)
  let neg k ((s,lb,ub) as si) =
    check_reduced k si;
    if lb <> extend k (highbit k) then
      (s, I.neg ub, I.neg lb)
    else if lb = ub then 
      single (mini k)
    else
      top k

  let neg = if debug then renormun neg else neg
	
  (** Subtractionf of strided intervals *)
  let sub k a b =
    add k a (neg k b)


  let minor k a b c d =
    let rec loop m =
      let cont() = loop (I.shift_right_logical m 1) in
	if m = 0L then a |% c
	else if bnot a &% c &% m <> 0L then
	  let temp = (a |% m ) &% I.neg m in
	    if int64_ucompare temp b <= 0 then
	      temp |% c
	    else cont()
	else if a &% bnot c &% m  <> 0L then
	  let temp = (c +% m) &% I.neg m in
	    if int64_ucompare temp d <= 0 then
	      temp  |% a
	    else cont()
	else
	  cont()
    in
      loop (highbit k)

  let maxor k a b c d =
    let rec loop m =
      let cont() = loop (I.shift_right_logical m 1) in
	if m = 0L then b |% d
	else if b &% d &% m <> 0L then
	  let temp1 = (b -% m) |% (m -% 1L) in
	  let temp2 = (d -% m) |% (m -% 1L) in
	    if int64_ucompare temp1 a >= 0 then
	      temp1 |% d
	    else if int64_ucompare temp2 c >= 0 then
	      temp2 |% b
	    else cont()
	else
	  cont()
    in
      loop (highbit k)

  let ntz x =
    let y = I.neg x &% (x -% 1L) in
    let rec bits n y =
      if y = 0L then n else bits (n+1) (I.shift_right y 1)
    in
      bits 0 y


  (** Bitwise OR *)
  let logor k ((s1,lb1,ub1) as a) ((s2,lb2,ub2) as b) =
    check_reduced2 k a b;
    let t = min (ntz s1) (ntz s2) in
    let s' = I.shift_left 1L t in
    let lowbits = (lb1 |% lb2) &% (s' -% 1L) in
    let (lb', ub') = match (lb1 < 0L, ub1 < 0L, lb2 < 0L, ub2 < 0L) with
      | (true, true, true, true)
      | (true, true, false, false)
      | (false, false, true, true)
      | (false, false, false, false) ->
	  (minor k lb1 ub1 lb2 ub2, maxor k lb1 ub1 lb2 ub2)
      | (true, true, true, false) ->
	  (lb1, -1L)
      | (true, false, true, true) ->
	  (lb2, -1L)
      | (true, false, true, false) ->
	  (min lb1 lb2, maxor k 0L ub1 0L ub2)
      | (true, false, false, false) ->
	  (minor k lb1 (-1L) lb2 ub2, maxor k 0L ub1 lb2 ub2)
      | (false, false, true, false) ->
	  (minor k lb1 ub1 lb2 (-1L), maxor k lb1 ub1 lb2 ub2)
      | _ -> failwith "Impossible: check_reduced prevents this"
    in
    let highmask = bnot(s' -% 1L) in
      (s', (lb' &% highmask) |% lowbits, (ub' &% highmask) |% lowbits)
      

  let logor = renormbin logor

  (** Bitwise NOT *)
  let lognot (_k:int) (s,l,u) =
    (s, bnot u, bnot l)

  let lognot = if debug then renormun lognot else lognot


  (** Bitwise AND *)
  let logand k x y =
    lognot k (logor k (lognot k x) (lognot k y))

  (** Bitwise XOR *)
  let logxor k x y =
    let n = lognot k
    and o = logor k in
    o (n(o (n x) y)) (n(o x (n y)))

  (** FIXME: Signed or unsigned modulus? *)
  let modulus k (s1,a,b) (s2,c,d) =
    if b = 0L then single 0L
    else
      (1L,0L, int64_umin b d)

  let modulus = renormbin modulus
	
(* shifting by more than k or by negative values
 * will be the same as shifting by k. *)
  let toshifts k =
    let f x = if x > Int64.of_int k || x < 0L then k else Int64.to_int x in
      function
	| (0L,x,y) ->
	    assert(x=y);
	    let s = f x in  (s,s)
	| (_s,x,y) ->
	    if x < 0L then
	      if y >= 0L then
		(* FIXME: using stride information could be useful here *)
		(0, k)
	      else (k,k)
	    else (* x >= 0L *)
	      (f x, f y)

  let mk_rshift shifter k ((s1,a,b) as x) y =
    check_reduced2 k x y;
    let (z1,z2) = toshifts k y
    and aa = trunc k a
    and bb = trunc k b
    and shift n z = extend k (shifter n z)in
    let a1 = shift aa z1
    and a2 = shift aa z2
    and b1 = shift bb z1
    and b2 = shift bb z2
    and s' = int64_umax (Int64.shift_right_logical s1 z2) 1L in
    let l = min (min a1 a2) (min b1 b2)
    and u = max (max a1 a2) (max b1 b2) in
      renorm k (s',l,u)

  (** Logical right-shift *)
  let rshift = mk_rshift Int64.shift_right_logical

  (** Arithmetic right-shift *)
  let arshift = mk_rshift Int64.shift_right

  (* construct these only once *)
  let yes = single (-1L)
  and no = single 0L
  and maybe = (1L, -1L, 0L)

  let eq k ((s1,a,b) as x) ((s2,c,d) as y) =
    check_reduced2 k x y;
    if a = b && a = c && a = d then
      yes
    else if b < c || d < a then
      no
    else
      let s' = uint64_gcd s1 s2 in
      let r1 = int64_urem a s'
      and r2 = int64_urem c s' in
	if r1 = r2 then
	  maybe
	else
	  no

  let union (s1,a,b) (s2,c,d) =
    let s' = uint64_gcd s1 s2 in
      if s' = 0L then
	if a = b && c = d then
	  let u = max a c
	  and l = min a c in
	    (u -% l, l, u)
	else failwith "union: strided interval not in reduced form"
      else 
	let r1 = I.rem a s' (* not right when s' is negative. *)
	and r2 = I.rem c s' in
	  if s' > 0L && r1 = r2 then
	    (s', min a c, max b d)
	  else (1L, min a c, max b d)

  let union x y =
    let (a,b,c) as res = union x y in
      if b = c then (0L,b,b) else (check_reduced 64 res; res)


  let rec fold f (s,a,b) init =
    if a = b then f a init
    else fold f (s, a+%s ,b) (f a init)

end (* module SI *)

(* Very simplified version of VSA, with no bounding *)
module SimpleVSA =
struct
  module DFP =
  struct
    module G = Cfg.SSA.G
    module L =
    struct
      type t = SI.t VM.t
      let top = VM.empty
      let equal = VM.equal (=)
      let meet x y =
	VM.fold
	  (fun k v res ->
	     try
	       let v' = VM.find k y in
	       let si = SI.union v v' in
		 VM.add k si res
	     with Not_found ->
	       VM.add k v res
	  )
	  x y
    end
    let s0 = G.V.create Cfg.BB_Entry
    let init g = L.top
    let dir = GraphDataflow.Forward

    let binop_to_si_function = function
      | PLUS -> SI.add
      | MINUS -> SI.sub
      | AND -> SI.logand
      | OR -> SI.logor
      | XOR -> SI.logxor
      | MOD -> SI.modulus
      | RSHIFT -> SI.rshift
      | ARSHIFT -> SI.arshift
      | EQ -> SI.eq
      | NEQ -> fun k x y -> SI.lognot k (SI.eq k x y)
      | TIMES
      | DIVIDE
      | SDIVIDE
      | SMOD
      | LSHIFT
      | LT
      | LE
      | SLT
      | SLE
	  -> failwith "unimplemented binop"

    let unop_to_si_function = function
      | NEG -> SI.neg
      | NOT -> SI.lognot

    let bits_of_val = function
      | Int(_,t) -> bits_of_width t
      | Var v -> bits_of_width (Var.typ v)
      | Lab _ -> 64 (* FIXME *)

    let rec transfer_stmt s l =
      match s with
	| Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
	| Assert _ | Jmp _ | CJmp _ | Label _ | Comment _
	| Halt _ ->
	    l
	| Move(v, e, _) ->
	    try
	      let top v = SI.top (bits_of_width(Var.typ v)) in
	      let find v = VM.find v l in
	      let do_find v =  try find v with Not_found -> top v  in
	      let val2si = function
		| Int(i,t) -> SI.of_bap_int (int64_of_big_int i) t
		| Lab _ -> raise(Unimplemented "No SI for labels (should be a constant)")
		| Var v -> do_find v
	      in
		try
		  let new_si = match e with
		    | Val x ->
			val2si x
		    | BinOp(op, x, y) ->
			let f = binop_to_si_function op in
			let k = bits_of_val x in
			let r = f k (val2si x) (val2si y) in
			  SI.check_reduced k r;
			  r
		    | UnOp(op, x) ->
			let f = unop_to_si_function op in
			let k = bits_of_val x in
			let r = f k (val2si x) in
			  SI.check_reduced k r;
			  r
		    | Phi(x::xs) ->
			List.fold_left
			  (fun i y -> SI.union i (do_find y))
			  (do_find x) xs
		    | Phi [] ->
			failwith "Encountered empty Phi expression"

(* This tries to preserve strides for loop variables, but takes too long, and
   wasn't working.
		    | Phi xs ->
			let res = List.fold_left
			  (fun res y ->
			     try let l = find y in
			       match res with None -> Some l
				 | Some l' -> Some(SI.union l l')
			     with Not_found -> res
			  )
			  None xs
			in
			  (match res with
			     | Some l -> l
			     | None -> raise Not_found
			  )
*)
		    | Cast(CAST_SIGNED, _, vl) ->
			val2si vl
(* This can result in non-reduced SIs currently 
		    | Cast(CAST_UNSIGNED, t, vl) ->
			let k = bits_of_val vl
			and k' = bits_of_width t
			and (s,a,b) as si = val2si vl in
			let d = 64-k in
			let f x = I.shift_right_logical(I.shift_left x d) d in
			  if k <> 64 && k <> k' then
			    (s, f a, f b)
			  else si
*)
		    | Cast(CAST_HIGH, t, vl) ->
			let k = bits_of_val vl
			and k' = bits_of_width t
			and (s,a,b) as si = val2si vl in
			let f x = I.shift_right x (k' - k) in
			  if k' <> k then
			    (s, f a, f b)
			  else si
		    | Cast(CAST_LOW, t, vl) ->
			raise(Unimplemented "FIXME")
		    | _ ->
			raise(Unimplemented "unimplemented expression type")
		  in
		    if try VM.find v l <> new_si with Not_found -> true then (
		      dprintf "adding %s = %s" (Pp.var_to_string v) (SI.to_string new_si);
		      VM.add v new_si l )
		    else l
		with Unimplemented _ | Invalid_argument _ ->
		  VM.add v (top v) l
	    with Invalid_argument _ | Not_found ->
	      l

    let transfer_function = fwd_transfer_stmt_to_block transfer_stmt

  end
  
  module DF = GraphDataflow.Make(DFP)

end (* module SimpleVSA *)



(** Value Sets *)
module VS =
struct
  type region = var (* FIXME? *)

  type address = region * SI.t

  type t = address list
      (* [] = (top, top, top,...), otherwise, any region not in the list
	 maps to bottom. *)

  let global = Var.newvar "internal only" (Reg 64) (* value doesn't really matter, so long as it's unique *)

  let top = []

  let pp_address p (r, si) =
    if r == global then p "$" else p(Pp.var_to_string r);
    p " |=> ";
    p (SI.to_string si)

  let pp p = function
    | [] -> p "VS.top"
    | x::xs ->
	p "(";
	pp_address p x;
	List.iter (fun x -> p ", "; pp_address p x) xs;
	p ")"
	

  let kind = function
    | [] -> `Top
    | [(r,_)] when r == global -> `VSglob
    | [_] -> `VSsingle
    | _ -> `VSarb

  let single x = [(global, SI.single x)]
  let of_bap_int i t = [(global, SI.of_bap_int i t)]

  let zero = [(global, SI.zero)]
  let one = [(global, SI.one)]
  let minus_one = [(global, SI.minus_one)]


  let add k x y = match (x,y) with
    | ([r2,si2],[r1,si1]) when r1 == global ->
	[(r2, SI.add k si1 si2)]
    | ([r,si1], xs) | (xs, [r,si1]) when r == global ->
	List.map (fun (r,si) -> (r, SI.add k si1 si)) xs
    | _ -> top

  let sub k x = function
    | [r,si] when r == global ->
	List.map (fun (r,si') -> (r, SI.sub k si' si)) x
    | _ -> top

	
  let makeother f id annihilator  k x y = match (x,y) with
    | ([r1,si1], [r2,si2]) when r1 == global && r1 == r2 ->
	[(r1, f k si1 si2)]
    | ([_] as vsg, vs) when vsg = id ->
	vs
    | (vs, ([_] as vsg))  when vsg = id ->
	vs
    | ([_] as vsg, _)  when Some vsg = annihilator ->
	BatOption.get annihilator
    | (_,([_] as vsg))  when Some vsg = annihilator ->
	BatOption.get annihilator
    | _ -> top

  let logand = makeother SI.logand minus_one (Some zero)

  let logor = makeother SI.logor zero (Some minus_one)

  let logxor = makeother SI.logxor zero None

  let yes = [(global, SI.yes)]
  let no = [(global, SI.no)]
  let maybe = [(global, SI.maybe)]

  (** Slightly unconservative equality checking. *)
  let eq k x y = match (x,y) with
     | ([r1,si1], [r2,si2]) when r1 == r2 ->
	 [(global, SI.eq k si1 si2)]
     | ([], _) | (_,[]) -> maybe
     | _ ->
	 if List.exists (fun(r,s)-> List.exists (fun(r2,s2)-> r == r2 && SI.eq k s s2 <> SI.no) y) x
	 then maybe
	 else no

  let union x y =
    let h = Hashtbl.create (List.length x + List.length y) in
    let add (r,si) =
      try Hashtbl.replace h r (SI.union si (Hashtbl.find h r))
      with Not_found ->
	Hashtbl.add h r si
    in
      List.iter add x;
      List.iter add y;
      Hashtbl.fold (fun k v r -> (k,v)::r) h []

  let fold f vs init =
    if vs = top then failwith "VS.fold doesn't work on Top"
    else
      List.fold_left (fun a (r,si) -> SI.fold (fun v -> f (r,v)) si a) init vs
end



(* Very simplified version of VSA, supporting regions *)
module RegionVSA =
struct
  module DFP =
  struct
    module G = Cfg.SSA.G
    module L =
    struct
      type t = VS.t VM.t
      let top = VM.empty
      let equal = VM.equal (=)
      let meet x y =
	VM.fold
	  (fun k v res ->
	     try
	       let v' = VM.find k y in
	       let vs = VS.union v v' in
		 VM.add k vs res
	     with Not_found ->
	       VM.add k v res
	  )
	  x y
    end
    let s0 =  G.V.create Cfg.BB_Entry
    let init g =
	VM.add sp [(sp, SI.zero)] L.top (* stack region *)

    let dir = GraphDataflow.Forward

    let binop_to_vs_function = function
      | PLUS -> VS.add
      | MINUS -> VS.sub
      | AND -> VS.logand
      | OR -> VS.logor
      | XOR -> VS.logxor
      | EQ -> VS.eq
      | TIMES
      | DIVIDE
      | SDIVIDE
      | MOD
      | SMOD
      | LSHIFT
      | RSHIFT
      | ARSHIFT
      | NEQ
      | LT
      | LE
      | SLT
      | SLE
	  -> raise(Unimplemented "unimplemented binop")

    let unop_to_vs_function _ = (raise(Unimplemented "unop") : int -> VS.t -> VS.t)


    let rec transfer_stmt s l =
      match s with
	| Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
	| Assert _ | Jmp _ | CJmp _ | Label _ | Comment _
	| Halt _ ->
	    l
	| Move(v, e, _) ->
	    try
	      let find v = VM.find v l in
	      let do_find v =  try find v with Not_found -> VS.top in
	      let val2vs = function
		| Int(i,t) -> VS.of_bap_int (int64_of_big_int i) t
		| Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)")
		| Var v -> do_find v
	      in
		try
		  let new_vs = match e with
		    | Val x ->
			val2vs x
		    | BinOp(op, x, y) ->
			let f = binop_to_vs_function op in
			let k = SimpleVSA.DFP.bits_of_val x in
			  f k (val2vs x) (val2vs y)
		    | UnOp(op, x) ->
			let f = unop_to_vs_function op in
			let k = SimpleVSA.DFP.bits_of_val x in
			  f k (val2vs x)
		    | Phi(x::xs) ->
			List.fold_left
			  (fun i y -> VS.union i (do_find y))
			  (do_find x) xs
		    | Phi [] ->
			failwith "Encountered empty Phi expression"
		    | Cast _ ->
			raise(Unimplemented "FIXME")
		    | _ ->
			raise(Unimplemented "unimplemented expression type")
		  in
		    VM.add v new_vs l
		with Unimplemented _ | Invalid_argument _ ->
		  VM.add v VS.top l
	    with Invalid_argument _ | Not_found ->
	      l

    let transfer_function = fwd_transfer_stmt_to_block transfer_stmt

  end
  
  module DF = GraphDataflow.Make(DFP)

end (* module RegionVSA *)

(** Abstract Store *)
module AllocEnv = struct
  type aloc = VS.region * int64
  module M1 = Map.Make(struct type t = VS.region let compare = Var.compare end)
  module M2 = Map.Make(struct type t = int64 let compare = Int64.compare end)

  (** This implementation may change... *)
  type t = VS.t M2.t M1.t

  let top = M1.empty

  (** Fold over all addresses in the AllocEnv *)
  let fold f ae i =
    M1.fold (fun r m2 a -> M2.fold (fun i vs a -> f (r,i) vs a) m2 a) ae i


  let read_concrete ae (r,i) =
    try M2.find i (M1.find r ae)
    with Not_found -> VS.top

  let read ae = function
    | [] -> VS.top
    | addrs -> (* FIXME: maybe shortcut this *)
	let res =
	  VS.fold
	    (fun v a ->  match a with
	       | None -> Some (read_concrete ae v)
	       | Some a -> Some(VS.union (read_concrete ae v) a))
	    addrs None
	in
	  match res with
	    | Some x -> x
	    | None -> failwith "AllocEnv.read impossible address"

  let write_concrete_strong ae (r,i) vl =
    if vl = VS.top then
      try
	let m2 = M1.find r ae in
	let m2' = M2.remove i m2 in
	  if M2.is_empty m2' then M1.remove r ae else M1.add r m2' ae
      with Not_found -> ae
    else
      let m2 = try M1.find r ae with Not_found -> M2.empty in
	M1.add r (M2.add i vl m2) ae

  let write_concrete_weak ae addr vl =
    write_concrete_strong ae addr (VS.union vl (read_concrete ae addr))

  let write ae addr vl =
    if addr = VS.top then
      if vl = VS.top then top
      else
	fold (fun k v a -> write_concrete_strong a k (VS.union vl v)) ae ae
    else
      match addr with
	| [(r, (0L,x,y))] when x = y ->
	   write_concrete_strong ae (r,x) vl
	| _ ->
	    VS.fold (fun v a -> write_concrete_weak a v vl) addr ae


  let union (x:t) (y:t) =
    fold (fun k v res -> write_concrete_weak res k v) x y

  let equal = M1.equal (M2.equal (=))

end

(** Abstract Environment *)
module AbsEnv = struct

  type value = [ `Scalar of VS.t | `Array of AllocEnv.t ]

  (** This implementation may change *)
  type t = value VM.t

  let empty = VM.empty

  let value_equal x y = match x,y with
    | (`Scalar x, `Scalar y) -> x = y
    | (`Array x, `Array y) -> AllocEnv.equal x y
    | _ -> false

  let equal x y =
    VM.equal (value_equal) x y



  let do_find_vs ae v =
    try match VM.find v ae with
      | `Scalar vs -> vs
      | _ -> VS.top
    with Not_found -> VS.top

  let ssaval2vs ae = function
    | Int(i,t) -> VS.of_bap_int (int64_of_big_int i) t
    | Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)")
    | Var v -> do_find_vs ae v

  let do_find_ae ae v =
    try match VM.find v ae with
      | `Array ae -> ae
      | _ -> AllocEnv.top
    with  Not_found -> AllocEnv.top
      
	
end  (* module AE *)




(** This does most of VSA, except the loop handling and special dataflow *)
module AlmostVSA =
struct
  module DFP =
  struct
    module G = Cfg.SSA.G
    module L =
    struct
      type t = AbsEnv.t
      let top = AbsEnv.empty
      let equal = AbsEnv.equal
      let meet (x:t) (y:t) =
	  VM.fold
	    (fun k v res ->
	       try
		 let v' = VM.find k y in
		 let vs = match v, v' with
		   | (`Scalar a, `Scalar b) -> `Scalar(VS.union a b)
		   | (`Array a, `Array b) -> `Array(AllocEnv.union a b)
		   | _ -> failwith "Tried to meet scalar and array"
		 in
		   VM.add k vs res
	       with Not_found ->
		 VM.add k v res
	    )
	    x y
    end
    let s0 =  G.V.create Cfg.BB_Entry
      
    (** Creates a lattice element that maps each of the given variables to
	it's own region. (For use as an inital value in the dataflow problem.)
    *)
    let init_vars vars =
      List.fold_left (fun vm x -> VM.add x (`Scalar [(x, SI.zero)]) vm) L.top vars

    let init g =
      init_vars [sp]

    let dir = GraphDataflow.Forward


    let rec transfer_stmt s l =
      match s with
	| Assert(Var _, _)  (* FIXME: Do we want to say v is true? *)
	| Assert _ | Jmp _ | CJmp _ | Label _ | Comment _
	| Halt _ ->
	    l
	| Move(v, e, _) ->
	    try
	      let find v = VM.find v l in
	      let do_find v =
		try match find v with
		  | `Scalar vs -> vs
		  | _ -> VS.top
		with Not_found -> VS.top
	      in
	      let val2vs = function
		| Int(i,t)->
		      VS.of_bap_int (int64_of_big_int i) t
		| Lab _ -> raise(Unimplemented "No VS for labels (should be a constant)")
		| Var v -> do_find v
	      in
	      let do_find_ae v =
		try match VM.find v l with
		  | `Array ae -> ae
		  | _ -> AllocEnv.top
		with  Not_found -> AllocEnv.top
	      in
	      match Var.typ v with
	      | Reg _ -> (
		  try
		    let new_vs = match e with
		      | Val x ->
			  val2vs x
		      | BinOp(op, x, y) ->
			  let f = RegionVSA.DFP.binop_to_vs_function op in
			  let k = SimpleVSA.DFP.bits_of_val x in
			    f k (val2vs x) (val2vs y)
		      | UnOp(op, x) ->
			  let f = RegionVSA.DFP.unop_to_vs_function op in
			  let k = SimpleVSA.DFP.bits_of_val x in
			    f k (val2vs x)
		      | Phi(x::xs) ->
			  List.fold_left
			    (fun i y -> VS.union i (do_find y))
			    (do_find x) xs
		      | Phi [] ->
			  failwith "Encountered empty Phi expression"
		      | Load(Var m, i, _e, _t) ->
			  (* FIXME: assumes deendianized.
			     ie: _e and _t should be the same for all loads and
			     stores of m. *)
			  AllocEnv.read (do_find_ae m) (val2vs i)
		      | Cast _ ->
			  raise(Unimplemented "FIXME")
		      | _ ->
			  raise(Unimplemented "unimplemented expression type")
		    in
		      VM.add v (`Scalar new_vs) l
		  with Unimplemented _ | Invalid_argument _ ->
		    VM.add v (`Scalar VS.top) l
		)
	      | TMem _ | Array _ -> (
		  try
		    let new_ae = match e with
		      | Val(Var x) ->
			  do_find_ae x
		      | Store(Var m,i,v,_e,_t) ->
			  (* FIXME: assumes deendianized.
			     ie: _e and _t should be the same for all loads and
			     stores of m. *)
			  AllocEnv.write (do_find_ae m) (val2vs i) (val2vs v)
		      | Phi(x::xs) ->
			  List.fold_left
			    (fun i y -> AllocEnv.union i (do_find_ae y))
			    (do_find_ae x) xs
		      | Phi [] ->
			  failwith "Encountered empty Phi expression"
		      | _ ->
			  raise(Unimplemented "unimplemented memory expression type")
		    in
		      VM.add v (`Array new_ae) l
		  with Unimplemented _ | Invalid_argument _ ->
		    VM.add v (`Array AllocEnv.top) l
		)
	    with Invalid_argument _ | Not_found ->
	      l

    let transfer_function = fwd_transfer_stmt_to_block transfer_stmt

  end
  
  module DF = GraphDataflow.Make(DFP)

end
