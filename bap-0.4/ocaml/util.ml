
(** Generally useful things.

    This module contains functions that are used in BAP, but which are
    not at all BAP specific.

    @author Ivan Jager
*)

open BatString
open BatList
open Big_int

(** The identity function *)
let id = fun x -> x

(** Curry a tupled function *)
let curry f = fun x y -> f(x,y)

(** The opposite of [curry] *)
let uncurry f = fun (x,y) -> f x y

(** [foldn f i n] is f (... (f (f i n) (n-1)) ...) 0 *)
let rec foldn ?(t=0) f i n =  match n-t with
  | 0 -> f i n
  | _ when n>t -> foldn ~t f (f i n) (n-1)
  | -1 -> i
  | _ -> raise (Invalid_argument "negative index number in foldn")

(** [foldn f i n] is f (... (f (f i n) (n-1)) ...) 0 *)
let rec foldn64 ?(t=0L) f i n =
  let (-) = Int64.sub in
  match n-t with
  | 0L -> f i n
  | _ when n>t -> foldn64 ~t f (f i n) (n-1L)
  | n when n == -1L -> i (* otags has trouble with '-1L' *)
  | _ -> raise (Invalid_argument "negative index number in foldn64")

(** [mapn f n] is the same as [f 0; f 1; ...; f n] *)
let mapn f n =
  List.rev (foldn (fun l i -> f(n-i)::l) [] n)
  (* List.rev is needed to make side effects happen in the same order *)


(** @return a union b, assuming a and b are sets *)
let list_union a b = 
  List.fold_left (fun acc x ->
		    if List.mem x b then acc else x::acc) b a
;;
  
(** @return a intersect b, assuming a and b are sets *)
let list_intersection a b =
  List.rev(List.fold_left (fun acc x ->
		    if List.mem x b then x::acc else acc) [] a)
;;

(** @return true if the intersection of two lists is not empty *)
let list_does_intersect a b =
  List.exists (fun x -> List.mem x a) b


(** Calls f with whichever of it's arguments is smaller first, and the longer
    one second. *)
let shortest_first f a b =
  let (la, lb) = (List.length a, List.length b) in
  if lb < la then f lb la else f la lb
    

(** @return a - b, assuming a and b are sets *)
let list_difference a b = 
    List.rev  (List.fold_left (fun acc x ->
			      if List.mem x b then
				acc
			      else
				x :: acc) [] a)
      
(** {list_subset a b} returns true when all elements in a are also in b *)
let list_subset a b =
  List.for_all (fun x -> List.mem x b) a

(** @return true when both sets contain the same elements *)
let list_set_eq a b = list_subset a b && list_subset b a


(** [union_find map items], where [map] is a mapping from items to
    their dependencies, finds independent elements in [items] *)
let union_find map items =
  let add_one res item =
    let set = map item in
    let (joined,indep) =
      List.partition (fun (s,is) -> list_does_intersect s set) res
    in
    let joined =
      List.fold_left
	(fun (s,is) (s2,is2) -> (list_union s2 s, List.rev_append is2 is))
	(set,[item]) joined
    in
    joined::indep
  in
  let res = List.fold_left add_one [] items in
  List.map snd res




(** Like [List.fold_left] but the arguments go in the same order as most fold functions.
    
    This exists, because I got fed up with fold_left being backwards.
*)
let list_foldl f l a = List.fold_left (fun i a -> f a i) a l

(** Pop the first element off a list ref. *)
let list_pop l =
  match !l with
  | x::xs -> l := xs; x
  | [] -> failwith "hd"

(** Push an element onto the front of a list ref. *)
let list_push l v =
  l := v :: !l

(** @return the last element of a list *)
let rec list_last = function
  | [x] -> x
  | _::x -> list_last x
  | [] -> raise (Invalid_argument("list_last expects non-empty list"))

(** @return (lst,last) where [lst] is the input lst minus 
    the last element [last]. @raise Invalid_argument if [lst] is empty *)
let list_partition_last lst = 
  match List.rev lst with
  | [x] -> ([],x)
  | x::ys -> (List.rev ys,x)
  | _ -> raise (Invalid_argument "list_partition_last expects non-empty list")

(** Like [list_last] but returns an option rather than failing *)
let list_last_option = function [] -> None | x -> Some(list_last x)

let list_filter_some f =
  let rec helper r l =
    match l with
    | x::xs -> helper (match f x with Some s -> s::r | None -> r) xs
    | [] -> List.rev r
  in
  helper []

let rec list_find_some f = function
  | x::xs -> (match f x with Some s -> s | None -> list_find_some f xs)
  | [] -> raise Not_found


(** [list_count f l] counts the number of items in [l] for which the
    predicate [f] is true. *)
let list_count f =
  List.fold_left (fun c x -> if f x then c+1 else c) 0

(** [list_unique l] returns a list of elements that occur in [l], without
    duplicates. (uses [=] and [Hashtbl.hash])  *)
let list_unique l =
  let h = Hashtbl.create (List.length l) in
  List.iter (fun x -> Hashtbl.replace h x ()) l;
  Hashtbl.fold (fun k () ul -> k::ul) h [] 

let rec split_common_prefix la lb = 
  match la,lb with
  | [], _ -> ([], la, lb)
  | _, [] -> ([], la, lb)
  | h1::t1, h2::t2 ->
      if h1 = h2 then
	let (a,b,c) = split_common_prefix t1 t2 in
	(h1::a, b, c)
      else ([], la, lb)

let split_common_suffix la lb =
  let (s,rla,rlb) = split_common_prefix (List.rev la) (List.rev lb) in
  (List.rev s, List.rev rla, List.rev rlb)

(** a composition operator. [(f <@ g) x] = [f(g x)] *)
let (<@) f g = (fun x -> f(g x))

(** Given Some(a), returns a. Given None, raises Not_found *)
let option_unwrap o =
  match o with
  | Some(x) -> x
  | None -> raise Not_found

(** Maps an ['a option] to a ['b option], given a function [f : 'a -> 'b] *)
let option_map f = function
  | None -> None
  | Some x -> Some(f x)

(** Map Some items and drop others from the list.
*)
let list_map_some f =
  let rec help res = function
    | [] -> List.rev res
    | x::xs -> help (match f x with Some i -> (i::res) | None -> res) xs
  in
    help []
  

let list_join f =
  function
    | x::(_::_ as xs) -> List.fold_left f x xs
    | [x] -> x
    | [] -> raise(Invalid_argument "list_join on empty list")

(** Get the keys from a hash table 
  If a key has multiple bindings, it is included once per binding *)
let get_hash_keys ?(sort_keys=false) htbl =
  let l = Hashtbl.fold (fun key data prev -> key::prev) htbl [] in
  if (sort_keys) then List.sort (Pervasives.compare) l
  else l

(** Change the extension of a filename *)
let change_ext filename new_ext =
  let last_dot_pos = String.rindex filename '.' in
  let base = String.sub filename 0 last_dot_pos in
  String.concat "." [base; new_ext]

(* Get list of file name in directory (ordered by name) *)
let list_directory ?(sort_files=true) dir_path =
  let file_type = Unix.stat dir_path in
  if (file_type.Unix.st_kind <> Unix.S_DIR) 
    then raise (Invalid_argument "Not a directory.");
  let file_array =
    try Sys.readdir dir_path 
    with _ -> raise (Invalid_argument "Could not read dir.")
  in
  if (sort_files) then Array.sort (Pervasives.compare) file_array;
  Array.to_list file_array


module HashUtil (H:Hashtbl.S) =
struct

  let hashtbl_eq ?(eq=(=)) h1 h2 =
    let subtbl h1 h2 =
      H.fold
	(fun k v r ->
	   try r && eq v (H.find h2 k)
	   with Not_found -> false )
	h1 true
    in
      subtbl h1 h2 && subtbl h2 h1
end

(* GRR, Hashtbl doesn't ascribe to the Hashtbl.S signature *)
let hashtbl_eq ?(eq=(=)) h1 h2 =
  let subtbl h1 h2 =
    Hashtbl.fold
      (fun k v r ->
	 try r && eq v (Hashtbl.find h2 k)
	 with Not_found -> false )
      h1 true
  in
    subtbl h1 h2 && subtbl h2 h1


module StringSet = Set.Make(String) ;;

let trim_newline s = 
  if String.length s > 0 && String.get s ((String.length s) -1) = '\n'
  then	String.sub s 0 ((String.length s)-2)
  else	s


		    
  
let apply_option f k = 
  match f with
  | None -> k
  | Some(f') -> f' k

let rec print_separated_list ps sep lst = 
  let rec doit acc = function
    | [] -> acc^""
    | x::[] -> acc^(ps x)
    | x::y::zs -> let acc = (ps x)^sep in
	(doit acc (y::zs))
  in
    doit "" lst


(* stuff that should be in Int64 *)


(** Unsigned comparison of int64 *)
let int64_ucompare x y =
  if x < 0L && y >= 0L then 1
  else if x >= 0L && y < 0L then -1
  else Int64.compare x y

(** Unsigned int64 division *)
let int64_udiv x y =
  (* Reference: Hacker's Delight (Warren, 2002) Section 9.3 *)
  if y < 0L
  then if int64_ucompare x y < 0 then 0L else 1L
  else if x < 0L
  then let all_but_last_bit =
    Int64.shift_left (Int64.div (Int64.shift_right_logical x 1) y) 1
  in
    if int64_ucompare (Int64.sub x (Int64.mul all_but_last_bit y)) y >= 0 then
      Int64.succ all_but_last_bit
    else
      all_but_last_bit
  else Int64.div x y


(** Unsigned int64 remainder *)
let int64_urem x y =
  Int64.sub x (Int64.mul y (int64_udiv x y))

(** Unsigned maxima of int64 *)
let int64_umax x y =
  if int64_ucompare x y > 0 then x else y

(** Unsigned minimum of int64 *)
let int64_umin x y =
  if int64_ucompare x y < 0 then x else y

(* end stuff that should be in Int64 *)

(** execute f with fd_from remapped to fd_to.
    useful for redirecting output of external code;
    e.g., redirecting stdout when calling STP code. *)
let run_with_remapped_fd fd_from fd_to f =
  (* remap *)
  let fd_to_saved = Unix.dup fd_to in
  Unix.dup2 fd_from fd_to;

  (* execute *)
  let rv = f () in

  (* restore *)
  Unix.dup2 fd_to_saved fd_to;
  Unix.close fd_to_saved;

  rv

let take = BatList.take
let fast_append = append

module StatusPrinter =
struct
  module D = Debug.Make(struct let name = "UtilStatus" and default=`Debug end)
  open D


  let updatetime = 5.0 (* update speed estimate every updatetime seconds *)
  let total = ref 0
  let current = ref 0
  let percentage = ref 0
  let last = ref 0
  let lasttime = ref 0.0
  let message = ref "Status"
  let starttime = ref 0.0

  let cpercent () =
    try
      (!current * 100 / !total)
    with Division_by_zero -> 0

  let rate () =
    let deltat = Unix.gettimeofday () -. !lasttime in
    let deltay = !current - !last in
      if deltat == 0.0 || deltay == 0 then
	-1.0
	  else
	(float_of_int deltay) /. deltat

  let update () = 
    let p = cpercent () in
    if p = -1 then
      if (debug) then Printf.printf "%s...\r" !message
    else
      if (debug) then Printf.printf "%s: %d%% (%f eps)\r" !message p (rate ()) ;

      percentage := p;
      last := !current;
      lasttime := Unix.gettimeofday();
    
      flush stdout
      
  let init msg size = 
    last := 0 ;
    current := 0 ;
    percentage := -1 ;
    message := msg ;
    total := size ;
    starttime := Unix.gettimeofday () ;
    lasttime := !starttime ;
    update ()
      
  let inc () =
    if !total != 0 then (
      current := !current + 1 ;
      let percentage' = cpercent() in
	if ((percentage' != !percentage) 
	    (*|| ((Unix.gettimeofday() -. !lasttime) >= updatetime)*) ) then
	  (update ()))
	  
  let stop () =
    if (debug) then 
	  Printf.printf "%s: Done! (%f seconds)\n" !message 
		(Unix.gettimeofday () -. !starttime) ;
    flush stdout
end

let has_some o = o <> None

(* list_firstindex l pred returns the index of the first list element
   that pred returns true on *)
let rec list_firstindex ?s:(s=0) l pred =
  match l with
  | [] -> raise Not_found
  | x::_ when pred x -> s
  | _::tl -> list_firstindex tl pred ~s:(s+1)

(* Insert elements in li to l before position n *)
let list_insert l li n =
  let rec list_split hl tl n =
    if n = 0 then 
      (List.rev hl,tl) 
    else
      let hl' = (List.hd tl) :: hl in
      let tl' = List.tl tl in
      let n' = n-1 in
      list_split hl' tl' n'
  in
  let hd,tl = list_split [] l n in
  hd @ li @ tl

(* Remove r elements in l starting at position s *)
let list_remove l s r =
  let aftere = s + r in
  let _,revl = List.fold_left
    (fun (i,l) e ->
       if i >= s && i < aftere then
	 (i+1,l)
       else
	 (i+1, e::l)
    ) (0,[]) l
  in
  List.rev revl

(** Compare two lists using f.  Compares pairs from both lists
    starting from the first elements using f, and returns the first
    non-zero result. If there is no non-zero result, zero is returned. *)
let list_compare f l1 l2 =
  let v = List.fold_left2
    (fun s e1 e2 ->
       match s with
       | Some(x) -> Some(x)
       | None -> 
	   let c = f e1 e2 in
	   (* If c is not zero, keep it *)
	   if c <> 0 then Some(c) else None
    ) None l1 l2 in
  match v with
  | None -> 0 (* Equal *)
  | Some(x) -> x (* Not equal *)

(** Same as {List.mem}, but uses a user-specified equality function. *)
let list_memf eqf ele l =
  try
    List.iter
      (fun e -> if eqf e ele then raise Exit else ()) l;
    false
  with Exit -> true

(** Given two lists, calls f with every possible combination *)
let list_cart_prod2 f l1 l2 =
  List.iter
    (fun x ->
       List.iter (f x) l2
    ) l1

(** Given three lists, calls f with every possible combination *)
let list_cart_prod3 f l1 l2 l3 =
  List.iter (fun x -> list_cart_prod2 (f x) l2 l3) l1

(** Given four lists, calls f with every possible combination *)
let list_cart_prod4 f l1 l2 l3 l4 =
  List.iter (fun x -> list_cart_prod3 (f x) l2 l3 l4) l1

(** {list_permutation setlist f} calls f with every value in the
    cartesian product of the sets in setlist. For instance, if setlist is
    [[1;2]; [2;3]], then this function will call f [1;2], f [1;3], f
    [2;2], and f [2;3] in some order.

    XXX: Replace implementation of list_card_prodn with this.
*)
let list_permutation setlist f =
  let newf =
    List.fold_left
      (fun (acc : 'a list -> unit) (set : 'a list) ->
         (fun l -> List.iter (fun o -> acc (o::l)) set)
      ) (fun l -> f l) setlist in
  newf []

(** Calls f on each element of l, and returns the first Some(x)
    returned.  If no Some(x) are returned, None is returned. *)
let list_existssome f l =
  (* Maybe list_find_some should call this instead *)
  try Some(list_find_some f l) with Not_found -> None

(** Calls f on each element of l and if there is Some() value returned
    for each list member, returns Some(unwrapped list). If at least
    one returns None, None is returned instead. *)
let list_for_allsome f l =
  let rec m r = function
    | [] -> Some(List.rev r)
    | x::xs -> match f x with
      | None -> None
      | Some x -> m (x::r) xs
  in
  m [] l

(** Deletes the first occurrence of e (if it exists) in the list
    and returns the updated list *)
let list_delete l e = 
  let rec delete_aux acc = function
    | [] -> List.rev acc
    | x::xs when e == x -> (List.rev acc)@xs
    | x::xs -> delete_aux (x::acc) xs
  in
    delete_aux [] l

(** Some -> true, None -> false *)
let has_some x = x <> None

(** Convert integer to binary represented as a string *)
let binary_of_int64 ?pad n = 
  let getb n = Int64.logand n 1L in (* Get lsb *)
  let getrest n = Int64.shift_right_logical n 1 in (* Get all but lsb *)
  let zeroextend s = match pad with
    | None -> s
    | Some(l) -> 
	let p = l - String.length s in
	assert (p >= 0);
	(String.make p '0') ^ s 
  in
  let rec f = function
    | 0L -> "0"
    | 1L -> "1"
    | n -> (f (getrest n)) ^ (f (getb n))
  in
  zeroextend (f n)

(* More substantial functions *)

(** Convert big integer to binary represented as a string *)
let binary_of_big_int ?pad n = 
  let getb n = Big_int.and_big_int n (big_int_of_int 1) in (* Get lsb *)
  let getrest n = Big_int.shift_right_big_int n 1 in (* Get all but lsb *)
  let zeroextend s = match pad with
    | None -> s
    | Some(l) -> 
	let p = l - String.length s in
	assert (p >= 0);
	(String.make p '0') ^ s 
  in
  let rec f = function
    | bi when (eq_big_int bi zero_big_int) -> "0"
    | bi when (eq_big_int bi unit_big_int) -> "1"
    | n -> (f (getrest n)) ^ (f (getb n))
  in
  zeroextend (f n)

(** Convert big integer to binary represented as a string

    XXX: We could make this more efficient by operating one int64 at a
    time, instead of just a nibble.
*)
let hex_of_big_int ?pad n = 
  let getn n = Big_int.and_big_int n (big_int_of_int 0xf) in (* Get lsnibble *)
  let getrest n = Big_int.shift_right_big_int n 4 in (* Get all but lsnibble *)
  let zeroextend s = match pad with
    | None -> s
    | Some(l) -> 
	let p = l - String.length s in
	assert (p >= 0);
	(String.make p '0') ^ s 
  in
  let (<=%) = le_big_int in
  let rec f = function
    | bi when bi <=% (big_int_of_int 0xf) -> Printf.sprintf "%x" (Big_int.int_of_big_int bi)
    | n -> (f (getrest n)) ^ (f (getn n))
  in
  zeroextend (f n)

(** Convert string representation in hex or decimal to big int form. *)
let big_int_of_string s =
  let hex_prefix = "0x" in
  let is_hex s =
    let re = Str.regexp ("^"^hex_prefix) in
    Str.string_match re s 0
  in
  let hex_to_bitlen s =
    (String.length s) * 4
  in
  let bitlen_to_hex n =
    (* Round up *)
    (n+3) / 4
  in
  (* If the highest bit is 1, Int64.of_string will return a negative
     value. So, we use 60 bits instead of 64 to avoid messing with
     int64's sign bit. *)
  let numbits = 60 in
  let getmost s = String.sub s 0 (bitlen_to_hex numbits) in
  let getrest s =
    let start = bitlen_to_hex numbits in
    let last = String.length s in
    String.sub s start (last - start)
  in
  (* Get rid of 0x prefix, if any *)
  let rec f s =
    let len = hex_to_bitlen s in
    if len <= numbits then
      let bi = Big_int.big_int_of_int64 (Int64.of_string ("0x"^s)) in
      let (>=%) = ge_big_int in
      assert (bi >=% zero_big_int);
      bi
    else (
      (* Printf.printf "getmost: %s v: %s\n" (getmost s) (Big_int.string_of_big_int (f (getmost s))); *)
      (* Printf.printf "getrest: %s v: %s\n" (getrest s) (Big_int.string_of_big_int (f (getrest s))); *)
      let (|%) = or_big_int in
      let (<<%) = shift_left_big_int in
      let bi = (f (getmost s) <<% (hex_to_bitlen (getrest s))) |% (f (getrest s)) in
      let (>=%) = ge_big_int in
      assert (bi >=% zero_big_int);
      bi
    )
  in
  if is_hex s then
    f (slice ~first:(String.length hex_prefix) s)
  else
    (* big_int_of_string handles decimals *)
    Big_int.big_int_of_string s
