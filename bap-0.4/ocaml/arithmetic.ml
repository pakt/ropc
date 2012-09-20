(** Basic integer arithmetic on N-bit integers

    These are common operations which are needed for constant folding or
    evaluation.

    @author Ivan Jager
 *)

module D = Debug.Make(struct let name = "Arithmetic" and default = `NoDebug end)
open Big_int
open Big_int_convenience
open D
open Type

exception ArithmeticEx of string

let bits_of_width = function
  | Reg n -> n
  | _ -> failwith "Expected register type"

(* drop high bits to type t *)
let to_big_int (i,t) =
  let bits = bits_of_width t in
  let modv = bi1 <<% bits in (* 2^bits *)
  let final = mod_big_int i modv in (* i mod 2^bits *)
  (* mod always returns a positive number *)
  final

(* sign extend to type t *)
let to_sbig_int (i,t) =
  let bits = bits_of_width t in
  let modv = bi1 <<% (bits-1) in (* 2^(bits-1) *)
  let final = mod_big_int i modv in (* i mod 2^(bits-1) *)
  (* mod always returns a positive number *)
  let sign = i >>% (bits-1) in
  if bi_is_zero sign then (* positive *) final else (* negative *) minus_big_int (modv -% (to_big_int (final, Reg(bits-1))))

(* signed truncating division implemented using euclidean division.

   See https://kestrel.ece.cmu.edu/svn/personal/edmcman/intdiv/ for an
   admittedly bad proof. Also see truncating_division_test in big_int
   test suite. *)
let t_div dividend divisor =
  if dividend >=% bi0 then
    (* When dividend >= 0, division is the same *)
    dividend /% divisor
  else
    (* If dividend < 0, we have to look at the remainder to figure out the answer. *)
    let (q,r) = quomod_big_int dividend divisor in
    if r ==% bi0 then
      (* If dividend < 0 and r = 0, then division is the same *)
      q
    else if r >=% bi0 then
      (* If dividend < 0 and r > 0, then truncateddivq = q - sign(q) *)
      q -% big_int_of_int (sign_big_int q)
    else raise (ArithmeticEx "t_div: If dividend < 0 then r can't be greater than 0!")

(* signed truncated modulus implemented using euclidean division. 

   See https://kestrel.ece.cmu.edu/svn/personal/edmcman/intdiv/ for an
   admittedly bad proof. Also see truncating_division_test in big_int
   test suite. *)
let t_mod dividend divisor =
  if dividend >=% bi0 then
    (* When dividend >= 0, division is the same *)
     dividend %% divisor
   else
     let r = dividend %% divisor in
     (* If r=0, the answer is r *)
     if r ==% bi0 then r
     else if r >=% bi0 then
       (* Otherwise it is r - |d| *)
       r -% (abs_big_int divisor)
    else raise (ArithmeticEx "t_mod: If dividend < 0 then r can't be greater than 0!")

(* shifting by more than the number of bits or by negative values
 * will be the same as shifting by the number of bits. *)
let toshift shiftedt v =
  let max = bits_of_width shiftedt
  and i = to_big_int v in
  assert (i >=% bi0);
  if i <=% (big_int_of_int max)
  then int_of_big_int i
  else
    (pdebug("shifting "^string_of_int max^"-bit value by "
	    ^string_of_big_int i);
     max)

(* "cast" an int64 to a value *)
let to_val t i =
  (to_big_int (i,t), t)

let exp_bool =
  let t = (unit_big_int, Reg(1))
  and f = (zero_big_int, Reg(1)) in
  (fun b -> if b then t else f)

(** [binop operand lhs lhst rhs rhst] *)
let binop op ((_,t) as v1) v2 =
  match op with
  | PLUS -> to_val t (add_big_int (to_big_int v1) (to_big_int v2))
  | MINUS -> to_val t (sub_big_int (to_big_int v1) (to_big_int v2))
  | TIMES -> to_val t (mult_big_int (to_big_int v1) (to_big_int v2))
  | AND -> to_val t (and_big_int (to_big_int v1) (to_big_int v2))
  | OR -> to_val t (or_big_int (to_big_int v1) (to_big_int v2))
  | XOR -> to_val t (xor_big_int (to_big_int v1) (to_big_int v2))
  | EQ -> exp_bool(eq_big_int (to_big_int v1) (to_big_int v2))
  | NEQ -> exp_bool(not (eq_big_int (to_big_int v1) (to_big_int v2)))
  | LSHIFT -> to_val t (shift_left_big_int (to_big_int v1) (toshift t v2))
  | RSHIFT -> to_val t (shift_right_big_int (to_big_int v1) (toshift t v2))
  | ARSHIFT -> to_val t (shift_right_towards_zero_big_int (to_sbig_int v1) (toshift t v2))
  | DIVIDE -> to_val t (div_big_int (to_big_int v1) (to_big_int v2))
  | SDIVIDE -> to_val t (t_div (to_sbig_int v1) (to_sbig_int v2))
  | MOD -> to_val t (mod_big_int (to_big_int v1) (to_big_int v2))
  | SMOD -> to_val t (t_mod (to_sbig_int v1) (to_sbig_int v2))
  | SLT -> exp_bool(lt_big_int (to_sbig_int v1) (to_sbig_int v2))
  | SLE -> exp_bool(le_big_int (to_sbig_int v1) (to_sbig_int v2))
  | LT -> exp_bool(lt_big_int (to_big_int v1) (to_big_int v2))
  | LE -> exp_bool(le_big_int (to_big_int v1) (to_big_int v2))


let unop op ((_,t) as v) =
  match op with
  | NEG -> to_val t (minus_big_int (to_big_int v))
  | NOT -> (* implemented as xor with -1 *)
    to_val t (xor_big_int (to_big_int (bim1,t)) (to_big_int v))

let cast ct ((_,t) as v) t2 =
  let bits1 = bits_of_width t
  and bits = bits_of_width t2 in
  (match ct with
   | CAST_UNSIGNED ->
       to_val t2 (to_big_int v)
   | CAST_SIGNED ->
       to_val t2 (to_sbig_int v)
   | CAST_HIGH ->
       to_val t2
	 (shift_right_big_int (to_big_int v) (bits1-bits))
   | CAST_LOW ->
       to_val t2 (to_big_int v)
  )


let extract h l ((_,t) as v) =
  let n = (h -% l) +% bi1 in
  let nt = Reg(int_of_big_int n) in
  let s = binop RSHIFT v (l,t) in
  cast CAST_LOW s nt


let concat ((_,lt) as lv) ((_,rt) as rv) =
  let bitsl,bitsr =
    match lt, rt with
    | Reg(bitsl), Reg(bitsr) -> bitsl, bitsr
    | _ -> failwith "concat"
  in
  let nt = Reg(bitsl + bitsr) in
  let lv = cast CAST_LOW lv nt in
  let rv = cast CAST_LOW rv nt in
  let lv = binop LSHIFT lv (biconst bitsr, lt) in
  binop OR lv rv


let is_zero ((i,t) as v) =
  let v = to_big_int v in
  sign_big_int v = 0
