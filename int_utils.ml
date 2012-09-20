(* we assume we are working on 32bit apps, so complain when it's not the case *)
let is_small64' x n = 
    let lim = Int64.shift_left Int64.one n in 
    let r = Int64.compare lim x in
    if r > 0 then true
    else assert false

let is_small64 x = is_small64' x 30 (* ocaml's INTs have 29 bits (30th is the sign bit *)

let int64_to_int32 x = 
    if is_small64' x 32 then
        Int64.to_int32 x
    else
        assert false

let safe_to_bigint x = 
    let y = Int64.of_int x in
    let _ = is_small64 y in
    Big_int.big_int_of_int x

let safe_to_int64 x = 
    let _ = is_small64 x in
    Int64.to_int x

let cast_range i j = 
    let i = Int64.of_int i in
    let j = Int64.of_int j in
    (i,j)

let i64_range_to_int i j = 
    let i = safe_to_int64 i in
    let j = safe_to_int64 j in
    (i,j)

let ast_i8 v = Ast.Int(safe_to_bigint v, Type.Reg 8)
let ast_i16 v = Ast.Int(safe_to_bigint v, Type.Reg 16)
let ast_i32 v = Ast.Int(safe_to_bigint v, Type.Reg 32)
let ast_i32_from_i32 v = Ast.Int(Big_int.big_int_of_int32 v, Type.Reg 32)

let neg x max = 
    let mask = Big_int.sub_big_int max (Big_int.unit_big_int) in (* naming consistency FTW :P *)
    let y = Big_int.xor_big_int x mask in (* NOT(x) *)
    let y = Big_int.succ_big_int y in (* +1 *)
    y

let sgn x max = 
    let sign_bit = Big_int.shift_right_big_int max 1 in
    let sign = Big_int.and_big_int x sign_bit in
    let s = Big_int.compare_big_int sign (Big_int.zero_big_int) in
    if s = 0 then 1
    else -1

let i32_of_big_int x = 
    let max = Big_int.shift_left_big_int (Big_int.unit_big_int) 32 in
    let _ = if Big_int.compare_big_int max x > 0 then true else assert false in
    let sign = sgn x max in
    let x = 
        if sign = -1 then
            neg x max
        else
            x
    in
    let x = Big_int.int32_of_big_int x in
    let x = Int32.mul x (Int32.of_int sign) in
    x

