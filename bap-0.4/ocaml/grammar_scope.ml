(** Scope module for parsing.

    ejs: I moved this out of the grammar.mly file so that external
    users could access it.
*)

(** Whether or not to strip the trailing _number from variable names *)
let strip_nums = ref false

let stripnum =
  let stripnum1 = Str.replace_first (Str.regexp "_[0-9]+$") "" in
  fun x ->
    if !strip_nums then
      stripnum1 x
    else
      x

let err s =
  prerr_endline s;
  failwith ("Parser: "^s)
    (*raise Parsing.Parse_error*)

module Scope = struct
  type t = (string, Var.t) Hashtbl.t * string Stack.t

  let create decls =
    let h = Hashtbl.create 5700 in
    List.iter (fun v -> Hashtbl.add h (Var.name v) v) decls;
    (h, Stack.create() )

  let defscope () = create Asmir.all_regs
  let cur_scope = ref (defscope ())

  let add n t =
    let v = Var.newvar (stripnum n) t in
    Hashtbl.add (fst !cur_scope) n v;
    v

  let add_push n t =
    Stack.push n (snd !cur_scope);
    add n t

  let pop () =
    let (h,s) = !cur_scope in
    let n = Stack.pop s in
    Hashtbl.remove h n


  let get_lval n t =
    let n = stripnum n in
    try
      let v = Hashtbl.find (fst !cur_scope) n in
      if t = None || t = Some(Var.typ v)
      then v
      else err ("Variable '"^n^"' used with inconsistent type")
    with Not_found ->
      (* Printf.printf "%s not found\n" n; *)
      match t with
      | Some t -> add n t
      | None -> err ("Type was never declared for '"^n^"'")

end

let get_scope () = !Scope.cur_scope
let set_scope s = Scope.cur_scope := s
let default_scope = Scope.defscope
let reset_scope () = set_scope (default_scope ())

(* let scope_create = Scope.create *)
(* let scope_set s = Scope.cur_scope := s *)
(* let scope_default = Scope.defscope *)

