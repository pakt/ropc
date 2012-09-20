(** Convert memory style accesses to array accesses.

   This modules converts all TMem references to normalized array references.

    @author Edward J. Schwartz
*)

(* TODO: Handle different endianness.  Use the type of the array expression *)

(* XXX: What should we do when there is a 1 bit access? *)

module D = Debug.Make(struct let name="memory2array" and default=`Debug end)
open D

open Ast
open Big_int
open Type
open Var


(** How big is normalized index? *)
let bitwidth = 8

let getwidth regtyp =
  match regtyp with
  | Reg(n) -> assert (n mod bitwidth = 0); n/bitwidth
  | _ -> failwith "Only support register indices!"

let split_load array index eletype endian bytenum =
  let newtype = Reg(bitwidth) in
  let indextype = Typecheck.infer_ast index in
  let indexplus = BinOp(PLUS, index, Int(big_int_of_int bytenum, indextype)) in
  let exp = Load(array, indexplus, endian, newtype) in
  let exp = Cast(CAST_UNSIGNED, eletype, exp) in
  let exp = exp_shl exp (Int(big_int_of_int (bytenum * bitwidth), eletype)) in
  exp

let split_load_list array index eletype endian =
  assert (full_exp_eq endian exp_false);
  let elesize = getwidth eletype in
  let mvar = newvar "loadnorm" (Array(eletype, Reg(bitwidth))) in
  (Util.mapn (split_load (Var mvar) index eletype endian) (elesize - 1), mvar)

let split_loads array index eletype endian =
  let (singlereads, mvar) = split_load_list array index eletype endian in
  let orexp = List.fold_left exp_or (List.hd singlereads) (List.tl singlereads) in
  Let(mvar, array, orexp)

let split_write array index eletype endian data bytenum =
  let newtype = Reg(bitwidth) in
  let indextype = Typecheck.infer_ast index in
  let indexplus = BinOp(PLUS, index, Int(big_int_of_int bytenum, indextype)) in
  let exp = exp_shr data (Int(big_int_of_int (bytenum * bitwidth), eletype)) in
  let exp = Cast(CAST_LOW, newtype, exp) in
  let exp = Store(array, indexplus, exp, endian, newtype) in
  exp

let split_write_list array index eletype endian data =
  assert (full_exp_eq endian exp_false);
  let inftype = Typecheck.infer_ast array in
  let tempmemvar = newvar "tempmem" inftype in
  let tempvalvar = newvar "tempval" eletype in
  let elesize = getwidth eletype in
  let singlewrites = Util.mapn (split_write (Var tempmemvar) index eletype endian (Var tempvalvar)) (elesize - 2) in
  (singlewrites @ [(split_write array index eletype endian (Var tempvalvar) (elesize - 1))], tempmemvar, tempvalvar)

let split_writes array index eletype endian data =
  let (singlewrites, tempmemvar, tempvalvar) = split_write_list array index eletype endian data in
  let letexp = List.fold_left (fun expr new_expr -> Let(tempmemvar, new_expr, expr)) (Var tempmemvar) singlewrites in
  Let(tempvalvar, data, letexp)


(** This visitor maps each TMem to an array *)
class memory2array_visitor () =
  let hash = VarHash.create 1000 in
  object (self)
    inherit Ast_visitor.nop

    method visit_avar avar =
      match Var.typ(avar) with
      |	TMem(idxt) ->
	  let array =
	    try VarHash.find hash avar
	    with Not_found ->
	      (* djb: we want the indx type to be the same. The
		 element type changes *)
	      let newarrvar = newvar (Var.name avar) (Array(idxt,Reg(bitwidth)))
	      in
	      VarHash.add hash avar newarrvar;
	      newarrvar
	  in
          `ChangeToAndDoChildren array (* Do we need to recurse on the avar? *)
      |	_ ->  `DoChildren


    method visit_rvar = self#visit_avar


    method visit_exp exp =
(*       Printf.printf "Visiting expression %s\n" (Pp.ast_exp_to_string exp); *)
      match exp with
      | Load(arr,idx,endian,t) -> ((* Printf.printf "Load %s\n" (Pp.ast_exp_to_string exp); *)
	  let width = (getwidth t) in
	  match width with
	  | 1 -> (* Printf.printf "Cool\n"; *)
	      `DoChildren
	  | _ -> (* Printf.printf "Need to split\n"; *)
	      let arr = Ast_visitor.exp_accept self arr in
	      let newexpr = split_loads arr idx t endian
	      in
	      (* Printf.printf "New Load %s\n" (Pp.ast_exp_to_string newexpr); *)
	      (* djb: still need to descend into children *)
	      `ChangeToAndDoChildren newexpr)
      | Store(arr,idx,data,endian,t) -> ((* Printf.printf "Store %s %s %s Reg%d\n" (Pp.ast_exp_to_string arr) (Pp.ast_exp_to_string idx) (Pp.ast_exp_to_string data) (getwidth t); *)
          let width = (getwidth t) in
          match width with
          | 1 -> (* Printf.printf "Cool!\n"; *)
	      `DoChildren
          | _ -> (* Printf.printf "Need to split\n"; *)
	      let arr = Ast_visitor.exp_accept self arr in
              let newexpr = split_writes arr idx t endian data in
	      `ChangeToAndDoChildren newexpr
        )
      | _ -> `DoChildren
  end


(** deend your average program. Returns new program where all memory
    broken down to byte-level reads and writes using array variables
    with the same name as the old memory variables.  *)
let coerce_prog prog =
  let visitor = new memory2array_visitor () in
  Ast_visitor.prog_accept visitor prog
