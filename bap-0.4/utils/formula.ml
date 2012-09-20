
let sort = 
  let underscore = Str.regexp_string "_" in
  let sort_aux (var1, _) (var2,_) =
    let ss1 = Str.split underscore var1 
    and ss2 = Str.split underscore var2 in
    let s1 = List.nth ss1 1 
    and	s2 = List.nth ss2 1 in      
      let s1i = int_of_string s1
      and s2i = int_of_string s2 in
      compare s1i s2i
  in  
  List.sort sort_aux

let main () =
  let cin = 
    if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
    else stdin
  in
  let is_input v = String.sub v 0 4 = "symb" in
  let lexbuf = Lexing.from_channel cin in
  let values = match Stp_grammar.main Stp_lexer.token lexbuf with
    | Some(x) -> x
    | None -> failwith "Unable to satisfy formula"
  in
  let values = List.filter (fun (v,_) -> is_input v) values in
  let sorted = sort values in
    List.iter (fun (_, num) -> Printf.printf "\\x%02Lx" num) sorted

;;

main ()
