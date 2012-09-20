(** Dataflow module for use with the ocamlgraph library

    @author Ivan Jager
*)

open Util
module List = BatListFull.List

module type G =
sig
  type t
  module V : Graph.Sig.COMPARABLE
  val pred : t -> V.t -> V.t list
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end


(** dataflow directions *)
type direction = Forward | Backward


module type BOUNDED_MEET_SEMILATTICE =
sig
  type t
  val top : t
  val meet : t -> t -> t
  val equal : t -> t -> bool
end

(** a dataflow is defined by a lattice over a graph. *)
module type DATAFLOW =
sig

  module L : BOUNDED_MEET_SEMILATTICE
  module G : G


  (** The transfer function over node elements, e.g., statements *)
  val transfer_function : G.t -> G.V.t -> L.t -> L.t
    
  (** the starting node for the analysis *)
  val s0 : G.V.t

  (** the initial value for analysis. This is what s0 should start
      out with. All other nodes start out with Top *)
  val init : G.t -> L.t

  (** the dataflow direction *)
  val dir : direction
end



module Make (D:DATAFLOW) = 
struct
  module H = Hashtbl.Make(D.G.V)

  (** returns a pair of functions for in(B) and out(B) for all nodes B
      in the graph *)
 let worklist_iterate ?(init = D.init) g = 
    let nodes = D.G.fold_vertex (fun x acc -> x::acc) g [] in
    let f_t = D.transfer_function g in 
    let succ,pred = match D.dir with
      | Forward ->
	  (D.G.succ g, D.G.pred g) 
      | Backward ->
	  (D.G.pred g, D.G.succ g)
    in
    let htin = H.create (List.length nodes) in
    let dfin = H.find htin in (* function to be returned *)
    let htout = H.create (List.length nodes) in
    let dfout n =
      try
	H.find htout n
      with Not_found ->
	let out = (f_t n (dfin n)) in
	H.add htout n out;
	out
    in
    List.iter (fun n -> H.add htin n D.L.top) nodes;
    H.replace htin D.s0 (init g);
    let rec do_work = function
      | [] -> ()
      | b::worklist ->  
	  let inset = (dfin b) in 
	  let outset = (f_t b inset) in 
	  H.replace htout b outset;
	  let affected_elems =
	    List.filter 
	      (fun s ->
		 let oldin = dfin s in
		 let newin = D.L.meet oldin (outset) in
		 if D.L.equal oldin newin
		 then false
		 else let () = H.replace htin s newin in true
	      )
	      (succ b)
	  in
	  let newwklist = worklist@list_difference affected_elems worklist
	  in
	  do_work newwklist
    in
    do_work [D.s0];
    (dfin, dfout)

end
