open BatListFull
open Type

(* a label map *)
module LM = Map.Make(struct type t = label let compare=compare end)

type bbid =
  | BB_Entry
  | BB_Exit
  | BB_Indirect
  | BB_Error
  | BB of int

let bbid_to_string = function
  | BB_Entry     -> "BB_Entry"
  | BB_Exit	 -> "BB_Exit"
  | BB_Indirect	 -> "BB_Indirect"
  | BB_Error	 -> "BB_Error"
  | BB n         -> "BB_"^string_of_int n

module BBid =
struct
  type t = bbid
  let compare = compare
  let hash = function
    | BB_Entry     ->  -1
    | BB_Exit	   ->  -2
    | BB_Indirect  ->  -3
    | BB_Error	   ->  -4
    | BB n         ->   n
  let equal = (=)
end

module BH = Hashtbl.Make(BBid)
module BM = Map.Make(BBid)



module E =
struct
  type t = bool option
  let compare = compare
  let default = None
end



module type CFG =
sig
  include Graph.Builder.S with type G.V.label = bbid and type G.E.label = bool option
  
  type lang

  val find_vertex : G.t -> G.V.label -> G.V.t
  val find_label : G.t -> Type.label -> G.V.t
  val get_stmts : G.t -> G.V.t -> lang
  val set_stmts : G.t -> G.V.t -> lang -> G.t
  val join_stmts : lang -> lang -> lang
  (*val newid : G.t -> bbid*)
  val create_vertex : G.t -> lang -> G.t * G.V.t

  (* extra builder-like stuff *)
  val remove_vertex : G.t -> G.V.t -> G.t
  val remove_edge : G.t -> G.V.t -> G.V.t -> G.t
  val remove_edge_e : G.t -> G.E.t -> G.t

end

type ('a,'b,'c) pcfg =
    {
      g: 'a;
      s: 'b;
      l: 'c;
      nextid : int
    }
    
module type Language =
sig 
  type t 
  val default : t 
  val join : t -> t -> t
  val iter_labels : (label->unit) -> t -> unit 
end

(* Begin persistent implementation *)
module MakeP (Lang: Language) =
struct
  (* A simple implementation for now... We can worry about optimizing later. *)
  (* FIXME: we really want a labeled bidirectional graph *)
  module G' = Graph.Persistent.Digraph.ConcreteLabeled(BBid)(E)

  type lang = Lang.t



  module G = struct

    module V = G'.V
    type vertex = V.t
    module E = G'.E
    type edge = E.t


(*    type t = {
      g: G'.t;
      s: Lang.t BM.t;
      l: V.t LM.t;
      nextid : int
    }
*)
    type t = (G'.t, Lang.t BM.t, V.t LM.t) pcfg

    let is_directed = true


    (* boring wrappers *)
      
    let is_empty     x = G'.is_empty x.g
    let nb_vertex    x = G'.nb_vertex x.g
    let nb_edges     x = G'.nb_edges     x.g
    let out_degree   x = G'.out_degree	 x.g
    let in_degree    x = G'.in_degree	 x.g
    let mem_vertex   x = G'.mem_vertex	 x.g
    let mem_edge     x = G'.mem_edge     x.g
    let mem_edge_e   x = G'.mem_edge_e   x.g
    let find_edge    x = G'.find_edge	 x.g  
    let find_all_edges   x = G'.find_all_edges  x.g
    let succ	     x = G'.succ	 x.g  
    let pred	     x = G'.pred	 x.g  
    let succ_e	     x = G'.succ_e	 x.g  
    let pred_e	     x = G'.pred_e       x.g

    let iter_vertex  x y = G'.iter_vertex x y.g
    let iter_edges   x y = G'.iter_edges  x y.g
    let fold_vertex  x y = G'.fold_vertex x y.g
    let fold_edges   x y = G'.fold_edges  x y.g
    let iter_edges_e x y = G'.iter_edges_e x y.g
    let fold_edges_e x y = G'.fold_edges_e x y.g 
    let iter_succ    x y = G'.iter_succ   x y.g	  
    let iter_pred    x y = G'.iter_pred   x y.g	  
    let fold_succ    x y = G'.fold_succ   x y.g 
    let fold_pred    x y = G'.fold_pred   x y.g
    let iter_succ_e  x y = G'.iter_succ_e x y.g
    let fold_succ_e  x y = G'.fold_succ_e x y.g
    let iter_pred_e  x y = G'.iter_pred_e x y.g
    let fold_pred_e  x y = G'.fold_pred_e x y.g


    let add_edge c v1 v2    = { c with g = G'.add_edge c.g v1 v2 }
    let add_edge_e c e      = { c with g = G'.add_edge_e c.g e }
    let remove_edge c v1 v2 = { c with g = G'.remove_edge c.g v1 v2 }
    let remove_edge_e c e   = { c with g = G'.remove_edge_e c.g e }
    let add_vertex c v      = { c with g = G'.add_vertex c.g v }
    
    let join_stmts = Lang.join





    (* Extra stuff to make this a CFG *)

    let find_vertex c id = 
      let v = V.create id in 
	if mem_vertex c v then v else raise Not_found
      
    let find_label c l = LM.find l c.l

    let get_stmts c v =
      try BM.find v c.s
      with Not_found -> Lang.default

    (* helper *)
    let fold_labels f l a =
      let r = ref a in
      Lang.iter_labels (fun l -> r := f l !r) l;
      !r
    let remove_labels c v =
      { c with
	  l = fold_labels (fun l lm -> LM.remove l lm) (get_stmts c v) c.l
      }

    let set_stmts c v s =
      let c = remove_labels c v in
      let sm = BM.add v s c.s in
      let lm = fold_labels (fun l lm -> LM.add l v lm) s c.l in
      { c with l=lm; s=sm }

    let newid c =
      let id = c.nextid in
      if id = -1 then failwith "newid: wrapped around";
      (BB id, {c with nextid = id + 1 })

    let create_vertex c stmts =
      let (i,c) = newid c in
      let v = V.create i in
      let c = add_vertex c v in
      (set_stmts c v stmts, v)

    (* Less boring wrappers *)
    let empty =
      {
	g = G'.empty;
	s = BM.empty;
	l = LM.empty;
	nextid = 0;
      }

    let remove_vertex c v =
      let c = remove_labels c v in
      let sm = BM.remove v c.s in
      let g = G'.remove_vertex c.g v in
      { c with g=g; s=sm }

    let map_vertex f c =
      failwith "map_vertex: unimplemented"

  end (* module G *)

  (* Copied from ocamlgraph's Builder.P so that G doesn't eat our extensions *)
  let empty () = G.empty
  let copy g = g
  let add_vertex = G.add_vertex
  let add_edge = G.add_edge
  let add_edge_e = G.add_edge_e

  (* extra, builder-like stuff *)
  let remove_vertex = G.remove_vertex
  let remove_edge = G.remove_edge
  let remove_edge_e = G.remove_edge_e

  let find_vertex = G.find_vertex 
  let find_label = G.find_label
  let get_stmts = G.get_stmts
  let set_stmts = G.set_stmts
  let join_stmts = G.join_stmts
  let newid = G.newid
  let create_vertex = G.create_vertex
end
(* end persistent implementation *)

module Make = MakeP

module LangAST =
struct
  type t = Ast.stmt list
  let default = []
  let join sl1 sl2 = match List.rev sl1 with
    | Ast.Jmp _ :: sl1' -> List.append (List.rev sl1') sl2
    | _ -> BatList.append sl1 sl2
  let iter_labels f =
    List.iter (function Ast.Label(l, _) -> f l  | _ -> () )
end

module LangSSA =
struct
  type t = Ssa.stmt list
  let default = []
  let join sl1 sl2 = match List.rev sl1 with
    | Ssa.Jmp _ :: sl1' -> List.append (List.rev sl1') sl2
    | _ -> BatList.append sl1 sl2
  let iter_labels f =
    (* optimization: assume labels are at the beginning *)
    let rec g = function
      | Ssa.Label(l,_) :: xs -> f l; g xs
      | Ssa.Comment _ :: xs -> g xs
      | _ -> ()
    in g
end

module AST = Make(LangAST)
module SSA = Make(LangSSA)


module type CFG_PRIV =
sig
  type lang
  module G' : Graph.Sig.G

  include Graph.Builder.S
    with type G.V.label = bbid
    and type G.E.label = bool option
    and type G.t = (G'.t, lang BM.t, G'.V.t LM.t) pcfg

  val get_stmts  : G.t -> G.V.t -> lang
  val set_stmts  : G.t -> G.V.t -> lang -> G.t
end

module MkMap(A:CFG_PRIV)(B:CFG_PRIV) =
struct
  let map f ({nextid=n} as cfg) =
    let s = B.empty() in
    let t vertex = B.G.V.create (A.G.V.label vertex) in
    let te e = B.G.E.create (t (A.G.E.src e)) (A.G.E.label e) (t (A.G.E.dst e)) in
    let per_vertex v g =
      let v' = t v in
      let g = B.add_vertex g v' in
      B.set_stmts g v' (f (A.get_stmts cfg v))
    in
    let s = A.G.fold_vertex per_vertex cfg s in
    let s = A.G.fold_edges_e (fun e s -> B.add_edge_e s (te e)) cfg s in
    { s with nextid = n}
end

module M2ssa = MkMap(AST)(SSA)
module M2ast = MkMap(SSA)(AST)

let map_ast2ssa = M2ssa.map
let map_ssa2ast = M2ast.map

