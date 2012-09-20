(** Weakest Precondition.

    These functions calculate the weakest precondition wp(p,q) for
    postcondition q and gcl program p.
*)

open Ast
open Type
open Gcl

module D = Debug.Make(struct let name = "WP" and default=`Debug end)
open D

module VH = Var.VarHash

let exp_or a b =
  if a == exp_false then b
  else if b == exp_false then a
  else exp_or a b

let exp_and a b =
  if a == exp_false || b == exp_false then exp_false
  else exp_and a b


(** wp(p,q), applying simp to simplify each intermediate expression
    during the calculation.  See "A Discipline of Programming" by
    Dijkstra, or CMU-CS-08-159 (Brumley's thesis), chapter 3.2.

    @param simp is an optional expression simplifier.

    @param p is the program
    
    @param q is the post-condition.
*)
let wp ?(simp=Util.id) (p:Gcl.t) (q:exp) : exp =
  (*  We use CPS to avoid stack overflows *)
  let rec wp q s k =
    match s with
    | Skip -> k q
    | Assume e ->
	k (simp(exp_implies e q))
    | Choice(s1, s2) ->
	wp q s1 (fun x -> wp q s2 (fun y -> k(simp(exp_and x y ))))
    | Seq(s1, s2) ->
	wp q s2 (fun x -> wp x s1 k)
    | Assign(t, e) ->
	k(simp(Let(t, e, q)))
    | Assert e -> 
	k (simp(exp_and e q))
  in
  wp q p  (fun x->x)




(** the efficient weakest precondition wp(p,q):Q where Q is guaranteed
    to be linear in the size of p.  This version expects p to be
    assignment-free, e.g., to be an SSA acyclic program. See
    CMU-CS-08-159.pdf (Brumley's thesis), chapter 3.3.  

    @param simp is an expression simplifier. You can pass in fun x->x
    if you want no simplification.  

    @param p is the program
    
    @param q is the post-condition.
*)
let efficient_wp ?(simp=Util.id) (p:Gcl.t) =
  let wlp_f_ctx = Hashtbl.create 113 in 
  let rec wlp_f s k =
    try k (Hashtbl.find wlp_f_ctx s)
    with Not_found ->
      let remember q = Hashtbl.add wlp_f_ctx s q; k q in
      match s with
      | Skip -> 
	  remember exp_false
      | Assume e
      | Assert e -> 
	  remember (exp_not e)
      | Choice(s1, s2) ->
	  wlp_f s1 (fun q1 -> wlp_f s2 
		      (fun q2 -> 
			 let q = simp(exp_and q1 q2 ) in
			 remember q
		      ))
      | Seq(s1, s2) ->
	  wlp_f s1 (fun q1 -> wlp_f s2 
		      (fun q2 -> 
			 let q = simp(exp_or q1 q2 ) in
			 remember q
		      ))
      | Assign _ -> 
	  raise (Invalid_argument("efficient_wp requires an assignment-free program"))
  in
  let rec wp_t s k : exp = 
    match s with
    | Skip
    | Assume _ ->
	k exp_true
    | Assert e ->
	k e
    | Choice(s1,s2) ->
	wp_t s1 (fun q1 -> wp_t s2 (fun q2 -> k(simp(exp_and q1 q2))))
(*	  (* by the book^Wthesis *)
    | Seq(s1, s2) ->
	wp_t s2 (fun q1 -> wp_t s1 
		   (fun q2 -> wlp_f s1
		      (fun q3 -> k(simp(exp_and q1 (exp_or q2 q3)))) ))
*)
	  (* by my own derivation *)
    | Seq(s1, s2) ->
	wp_t s1 (fun q1 -> wp_t s2
		   (fun q2 -> wlp_f s1
		      (fun q3 -> k(simp(exp_and q1 (exp_or q2 q3)))) ))
    | Assign _ -> 
	invalid_arg "efficient_wp requires an assignment-free program"
  in
  let q0 = wlp_f p (fun x -> x) in 
  let qpr = wp_t p (fun x -> x) in 
  (fun q -> simp(exp_and qpr (exp_or q0 q)))

let dwp_name = "dwptemp"
let dwp_name = "t"

let ast_size e =
  let s = ref 0 in
  let vis = object
    inherit Ast_visitor.nop
    method visit_exp _ =
      incr s;
      `DoChildren
  end in
  ignore(Ast_visitor.exp_accept vis e);
  !s


(* helper for dwp *)
let variableify k v e =
    if k <= 1 || ast_size e > k then
      let x = Var.newvar dwp_name (Typecheck.infer_ast e) in
      let xe = Var x in
      ((x, e) :: v, xe)
    else
      (v, e)

let rm_useless_vars vs n w =
  (* FIXME: remove vars that are only referenced once *)
  let l = List.length vs in
  let h = VH.create l
  and c = VH.create l in  
  List.iter (fun (v,e) -> VH.add h v e; VH.add c v (ref 0)) vs;
  let inc v = try incr (VH.find c v) with Not_found -> () in
  let counter =
    object(self)
      inherit Ast_visitor.nop
	(* FIXME: worry about Let? *)
      method visit_rvar r =
	inc r;
	`DoChildren
    end
  in
  ignore(Ast_visitor.exp_accept counter n);
  ignore(Ast_visitor.exp_accept counter w);
  List.iter (fun(v,e)-> ignore(Ast_visitor.exp_accept counter e)) vs;
  let to_remove v = try !(VH.find c v) <= 1 with Not_found -> false in
  let vs = List.filter (fun p -> not(to_remove (fst p))) vs in
  let subst =
    object(self)
      inherit Ast_visitor.nop
      method visit_exp = function
	| Var v when to_remove v ->
	    `ChangeToAndDoChildren(VH.find h v)
	| _ -> `DoChildren
    end
  in
  let n = Ast_visitor.exp_accept subst n
  and w = Ast_visitor.exp_accept subst w
  and vs = List.map (fun (v,e) -> (v, Ast_visitor.exp_accept subst e)) vs in
  (vs,n,w)



let assignments_to_exp = function
  | [] -> None
  | v::vs ->
      let p2e (v,e) = BinOp(EQ, Var v, e) in
      let rec h e = function
	| a::rest ->
	    if true then h (exp_and e (p2e a)) rest
	    else h (exp_and (p2e a) e) rest
	| [] -> e
      in
      Some(h (p2e v) vs)

let dwp_help ?(simp=Util.id) ?(k=1) f (p:Gcl.t) =
  let g (v, n, w) =
    let rec g' v ns ws fn fw =
      match (ns,ws) with
      | (n::ns, w::ws) ->
	  let (v,n) = variableify k v n in
	  g' v ns ws (exp_and n fn) (exp_or w (exp_and n fw))
      | ([],[]) ->
	  (v, fn, fw)
      | _ -> failwith "n and w are supposed to have the same length"
    in
    match (n,w) with
    | (n::ns, w::ws) -> let (v,n) = variableify k v n in g' v ns ws n w
    | ([],[]) -> (v, exp_true, exp_false)
    | _ -> failwith "n and w are supposed to have the same length"
  in
  let (vs,n,w) = g (f g ([],[],[]) p) in
  (* Surprisingly enough, this is a toss up. It makes some formulas
     much easier for CVC and others much harder. *)
  (* let (vs,n,w) = rm_useless_vars vs n w in *)
  (assignments_to_exp vs, vs, n, w)


(** Generates a 1st order logic VC using the DWP algorithm. *)
let dwp_1st ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let f' g =
    let rec f ((v,n,w) as vnw) s = match s with
      | Assert e ->
	  let (v,e) = if less_duplication then variableify k v e else (v,e) in
	  (v, e::n, exp_not e :: w)
      | Assume e ->
	  (v, e::n, exp_false::w)
      | Seq(a, b) ->
	  let vnw' = f vnw a in (* FIXME: do we need tail recursion?*)
	  f vnw' b
      | Choice(a, b) ->
	  let (v,na,wa) = f (v,[],[]) a in
	  let (v,nb,wb) = f (v,[],[]) b in
	  let (v,na,wa) = g (v,na,wa) in
	  let (v,nb,wb) = g (v,nb,wb) in
	  (v, (exp_or na nb)::n, (exp_or wa wb)::w)
      | Skip ->
	  vnw
      | Assign _ ->
	  invalid_arg "aij_wp requires an assignment-free program"
    in
    f
  in
  let (vo, vs, n, w) = dwp_help ~simp ~k f' p in
  match vo with
  | Some v ->
      let vars = List.map fst vs in
      (fun q -> (vars, exp_implies v (exp_and (exp_not w) (exp_implies n q))))
  | None ->
      (fun q -> ([], exp_and (exp_not w) (exp_implies n q)))



let dwp_pred_help ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let f' g =
    let rec f ((v,n,w) as vnw) s = match s with
      | Assert e ->
	  let (v,e) = if less_duplication then variableify k v e else (v,e) in
	  (v, e::n, exp_not e :: w)
      | Assume e ->
	  (v, e::n, exp_false::w)
      | Seq(a, b) ->
	  let vnw' = f vnw a in (* FIXME: do we need tail recursion?*)
	  f vnw' b
      | Choice(a, b) ->
	  let (v,na,wa) = f (v,[],[]) a in
	  let (v,nb,wb) = f (v,[],[]) b in
	  let (v,na,wa) = g (v,na,wa) in
	  let (v,nb,wb) = g (v,nb,wb) in
	  (v, (exp_or na nb)::n, (exp_or wa wb)::w)
      | Skip ->
	  vnw
      | Assign _ ->
	  invalid_arg "aij_wp requires an assignment-free program"
    in
    f
  in
  dwp_help ~simp ~k f' p

(** Generates a predicate logic VC using the DWP algorithm. *)
let dwp ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let (vo, _, n, w) = dwp_pred_help ~simp ~less_duplication ~k p in
  match vo with
  | Some v ->
      (fun q -> (exp_and v (exp_and (exp_not w) (exp_and n q))))
  | None ->
      (fun q -> (exp_and (exp_not w) (exp_and n q)))


let dwp_let ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let (_, vars, n, w) = dwp_pred_help ~simp ~less_duplication ~k p in
  (fun q -> 
     let exp = exp_and (exp_not w) (exp_and n q) in
     List.fold_left (fun exp (v,e) -> Let(v,e,exp)) exp vars
  )

(*let dwp = dwp_let*)



(*let flanagansaxe_dumb ?(simp=Util.id) (p:Gcl.t) =
  let rec n = function
    | Assert e -> e
    | Assume e -> e
    | Seq(a,b) -> exp_and (n a) (n b)
    | Choice(a,b) -> exp_or (n a) (n b)
  in
  let rec w = function
    | Assert e -> exp_not e
    | Assume e -> exp_false
    | Seq(a,b) -> exp_or (w a) (exp_and (n a) (w b))
    | Choice(a,b) -> exp_or (w a) (w b)
  in
  let ws = w p and ns = n p in
  (fun q ->
     exp_and (exp_not ws) (exp_implies ns q)
  )
*)

let flanagansaxe ?(simp=Util.id) ?(less_duplication=true) ?(k=1) (p:Gcl.t) =
  let rec nw v = function
    | Assume e -> (e, exp_false, v)
    | Assert e ->
	let (v,e) = if less_duplication then variableify k v e else (v,e) in
	(e, exp_not e, v)
    | Choice(a,b) ->
	let (na,wa,v) = nw v a in
	let (nb,wb,v) = nw v b in
	(exp_or na nb, exp_or wa wb, v)
    | Seq(a,b) ->
	let (na,wa,v) = nw v a in
	let (v,na) = variableify k v na in
	let (nb,wb,v) = nw v b in
	(exp_and na nb, exp_or wa (exp_and na wb), v)
    | Assign _ -> failwith "gcl should be passified"
    | Skip -> (exp_true, exp_false, v)
  in
  let (ns,ws,v) = nw [] p in
  match assignments_to_exp v with
  | None ->
      (fun q ->	 exp_and (exp_not ws) (exp_implies ns q) )
  | Some v ->
      (fun q ->	 exp_and v (exp_and (exp_not ws) (exp_implies ns q)) )

