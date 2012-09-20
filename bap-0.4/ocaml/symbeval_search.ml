(** A module to try out search strategies on symbolic execution *)

module D = Debug.Make(struct let name = "SearchFSE" and default=`Debug end)
open D

open Symbeval

module type STRATEGY =
sig
  type t
  type data
  type initdata
  val start_at : ctx -> initdata -> t
  val pop_next : t -> ((ctx * data) * t) option
  val add_next_states : t -> ctx -> data -> ctx list -> t
end

module type Symb =
sig 
  val init : Ast.stmt list -> ctx
  val eval : ctx -> ctx list
end

module NaiveSymb =
struct
  let init = SymbolicSlow.build_default_context
  let eval = SymbolicSlow.eval
end

module FastSymb =
struct
  let init = Symbolic.build_default_context
  let eval = Symbolic.eval
end

module MakeSearch(S:STRATEGY)(Symbolic:Symb) =
struct
  let rec search post predicates q =
    match S.pop_next q with
    | None -> predicates
    | Some ((st,d),q) ->
	let (newstates, predicates) =
	  try (Symbolic.eval st, predicates) with
	  | Halted(v,s) ->
	      let q = symb_to_exp (eval_expr s.delta post) in
	      let pred = Ast.exp_and q s.pred in
	      ([], pred :: predicates)
	  | AssertFailed {pc=pc} ->
	      wprintf "failed assertion at %Ld\n" pc;
	      ([], predicates)  (* try other branches *)
	in
	let q = S.add_next_states q st d newstates in
	search post predicates q

  let eval_ast_program initdata prog post =
    let ctx = Symbolic.init prog in
    let predicates = search post [] (S.start_at ctx initdata) in
    if debug then dprintf "Explored %d paths." (List.length predicates);
    Util.list_join Ast.exp_or predicates

end

(** A purely functional queue with amortised constant time enqueue and dequeue. *)
module Q = struct
  (* Maybe put this in Util as FQueue? *)
  type 'a t = 'a list * 'a list

  let empty = ([],[])

  let enqueue (a,b) v = (a, v::b)

  let enqueue_all (a,b) l =
    (a, List.rev_append l b)

  let dequeue = function
    | (v::a, b) -> (v, (a,b))
    | ([], b) ->
	match List.rev b with
	| v::a -> (v, (a,[]))
	| [] -> raise Queue.Empty

  let dequeue_o q =
    try Some(dequeue q) with Queue.Empty -> None
end



module UnboundedBFS = MakeSearch(
  struct
    type t = ctx Q.t
    type data = unit
    type initdata = unit
    let start_at s () = Q.enqueue Q.empty s
    let pop_next q = match Q.dequeue_o q with
      | Some(st,q) -> Some((st,()),q)
      | None -> None
    let add_next_states q st () newstates = Q.enqueue_all q newstates
  end)
  (* Perhaps these can be automated *)
module UnboundedBFSNaive = UnboundedBFS(NaiveSymb)
module UnboundedBFSFast = UnboundedBFS(FastSymb)
let bfs_ast_program p q = UnboundedBFSNaive.eval_ast_program () p q
let bfs_ast_program_fast p q = UnboundedBFSFast.eval_ast_program () p q


module MaxdepthBFS = MakeSearch(
  struct
    type data = int
    type initdata = int
    type t = (ctx * data) Q.t
    let start_at s i = Q.enqueue Q.empty (s,i)
    let pop_next = Q.dequeue_o
    let add_next_states q st i newstates =
      if i > 0 then
	List.fold_left (fun q c -> Q.enqueue q (c, i-1)) q newstates
      else
	q
  end)
module MaxdepthBFSNaive = MaxdepthBFS(NaiveSymb)
module MaxdepthBFSFast = MaxdepthBFS(FastSymb)
let bfs_maxdepth_ast_program = MaxdepthBFSNaive.eval_ast_program
let bfs_maxdepth_ast_program_fast = MaxdepthBFSFast.eval_ast_program

module UnboundedDFS = MakeSearch(
  struct
    type t = ctx list
    type data = unit
    type initdata = unit
    let start_at s () = [s]
    let pop_next = function
      | st::rest -> Some((st,()),rest)
      | [] -> None
    let add_next_states q st () newstates = newstates @ q
  end)
module UnboundedDFSNaive = UnboundedDFS(NaiveSymb)
module UnboundedDFSFast = UnboundedDFS(FastSymb)
let dfs_ast_program p q = UnboundedDFSNaive.eval_ast_program () p q
let dfs_ast_program_fast p q = UnboundedDFSFast.eval_ast_program () p q

module MaxdepthDFS = MakeSearch(
  struct
    type data = int
    type initdata = int
    type t = (ctx * data) list
    let start_at s i = [(s,i)]
    let pop_next = function
      | st::rest -> Some(st,rest)
      | [] -> None
    let add_next_states q st i newstates =
      if i <= 0 then q
      else
	let ni = i-1 in
	List.fold_left (fun q s -> (s,ni)::q) q newstates
  end)
module MaxdepthDFSNaive = MaxdepthDFS(NaiveSymb)
module MaxdepthDFSFast = MaxdepthDFS(FastSymb)
let dfs_maxdepth_ast_program = MaxdepthDFSNaive.eval_ast_program
let dfs_maxdepth_ast_program_fast = MaxdepthDFSFast.eval_ast_program


module EdgeMap = Map.Make(struct type t = int64 * int64 let compare = compare end)

(* DFS excluding paths that visit the same point more than N times. *)
module MaxrepeatDFS = MakeSearch(
  struct
    type data = int EdgeMap.t
    type initdata = int
    type t = (ctx * data) list * int
    let start_at s i = ([(s,EdgeMap.empty)], i)
    let pop_next = function
      | (st::rest, i) -> Some(st,(rest,i))
      | ([], _) -> None
    let add_next_states ((q,i) as l) st m = function
      | [] -> l
      | [st] -> ((st,m)::q, i)
      | newstates ->
	  let addedge nst =
	    let edge = (st.pc, nst.pc) in
	    let count = try EdgeMap.find edge m with Not_found -> 0 in
	    if count >= i then None
	    else Some(nst, EdgeMap.add edge (count+1) m)
	  in
	  let newstates = Util.list_map_some addedge newstates in
	  (newstates@q, i)
  end)
module MaxrepeatDFSNaive = MaxrepeatDFS(NaiveSymb)
module MaxrepeatDFSFast = MaxrepeatDFS(FastSymb)
let maxrepeat_ast_program = MaxrepeatDFSNaive.eval_ast_program
let maxrepeat_ast_program_fast = MaxrepeatDFSFast.eval_ast_program

let rec conceval_ast_program ctx =
  try
    match (Symbolic.eval ctx) with
    | [next] -> conceval_ast_program next
    | _ -> failwith "More than one state returned in concrete execution!"
  with _ -> ctx
