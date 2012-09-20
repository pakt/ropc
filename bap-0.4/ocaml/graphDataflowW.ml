(** Dataflow module for use with the ocamlgraph library
    + widening
*)

open Util
module List = BatListFull.List

module D = Debug.Make(struct let name = "GraphDataflowW" and default=`Debug end)
(* module D = Debug.Make(struct let name = "GraphDataflowW" and default=`NoDebug end) *)

open D

module type SE =
sig
  type t

  type label = bool option
  type vertex = Cfg.SSA.G.V.t
  val src : t -> vertex
  val dst : t -> vertex
  val create : vertex -> label -> vertex -> t
  val label : t -> label
end

module type SV =
sig
  type t = Cfg.SSA.G.V.t
  type label = Cfg.bbid
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  val label : t -> label
end


module type SSAG =
sig
  (* module V : Graph.Sig.COMPARABLE *)

  type t 

  module V : SV
  module E : SE

  val pred : t -> V.t -> V.t list
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val succ_e : t -> V.t -> E.t list 
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val is_directed : bool

end

(** dataflow directions *)
type direction = Forward | Backward

type outset_cond_t = OutTrue|OutFalse|OutNone

module type BOUNDED_MEET_SEMILATTICE_WIDENING =
sig
  type t
  val top : t
  val meet : t -> t -> t
  val widen : t -> t -> t
  val equal : t -> t -> bool

end

(** a dataflow is defined by a lattice over a graph. *)
module type DATAFLOW_WIDENING =
sig

  module L : BOUNDED_MEET_SEMILATTICE_WIDENING
  module G : SSAG


  (** The transfer function over node elements, e.g., statements *)
  val transfer_function : ?cond:outset_cond_t -> G.t -> G.V.t -> L.t -> L.t
    
  (** the starting node for the analysis *)
  val s0 : G.V.t

  (** the initial value for analysis. This is what s0 should start
      out with. All other nodes start out with Top *)
  val init : L.t

  (** the dataflow direction *)
  val dir : direction
end

module Make_widening (D:DATAFLOW_WIDENING) = 
struct

  module LH = Hashtbl

  module H = Hashtbl.Make(D.G.V)


  (** worklist_iterate with widening *)
 let worklist_iterate_w_widening ?(init = D.init) ?(s0 = D.s0) ?(loop_heads:D.G.V.label list option  = None) g = 
    let nodes = D.G.fold_vertex (fun x acc -> x::acc) g [] in
    let f_t = D.transfer_function ~cond:OutNone g in 
    let succ,pred = match D.dir with
      | Forward ->
	  (D.G.succ g, D.G.pred g) 
      | Backward ->
	  (D.G.pred g, D.G.succ g)
    in
    let htin = H.create (List.length nodes) in
    let dfin = H.find htin in (* function to be returned *)
    let htout = H.create (List.length nodes) in
    let count = H.create (List.length nodes) in
    let dfout n =
      try
	H.find htout n
      with Not_found ->
	let out = (f_t n (dfin n)) in
	H.add htout n out;
	out
    in
    let get_inset b =
      (dfin b)
    in
    (* let v2s v = Cfg.bbid_to_string (D.G.V.label v) in  *)
    let loophead = LH.create (List.length nodes) in

    let find_loopheads g = 
      let module DFS = Graph.Traverse.Dfs(D.G) in
      let count = ref( (List.length nodes) ) in
      let post_num = H.create (List.length nodes) in
      let _ = DFS.postfix_component (fun v ->
	if not (H.mem post_num v) then (
	  H.add post_num v !count;
	  decr count
	) else () 
      ) g s0
      in
      DFS.prefix_component (fun p ->
	D.G.iter_succ (fun v ->
	  let n_p = H.find post_num p in
	  let n_v = H.find post_num v in 
	  if n_v <= n_p then (
	    LH.add loophead (D.G.V.label v) () 
	  ) else ()
	) g p 
      ) g s0
    in

    let _ =
      match loop_heads with
      | Some lh ->
    	List.iter (fun x -> LH.add loophead x () ) lh ;
	()
      | None ->
    	  find_loopheads g 
    in

    let confluence b c_oldin c_outset =
      let visit_count =
      	try
      	  H.find count b
      	with Not_found ->
      	  0
      in
      let () = H.replace count b (visit_count + 1) in
      if LH.mem loophead (D.G.V.label b) then
	D.L.widen c_oldin c_outset
      else
	D.L.meet c_oldin c_outset
    in
    List.iter (fun n -> H.add htin n D.L.top) nodes;
    H.replace htin s0 init;
    let rec do_work = function
      | [] -> ()
      | b::worklist ->  
	  let inset = (get_inset b) in
	  let affected_elems =
	    let check_bb s out = 
	       let oldin = dfin s in
	       let newin = confluence s oldin (out) in
	       if D.L.equal oldin newin
	       then false
	       else let () = H.replace htin s newin in true
	    in
	    let outset = (f_t b inset) in 
	    let () = H.replace htout b outset in
	    List.filter 
	      (fun s ->
		check_bb s outset 
	      )
	      (succ b)
	  in
	  let newwklist = worklist@list_difference affected_elems worklist
	  in
	  do_work newwklist
    in
    do_work [s0];
    (dfin, dfout)
end
