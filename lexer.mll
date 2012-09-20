{
    open Ast
    open Common
    open Parser
    open Printf
    open Scanf
    open Lexing


    let keyword_table = 
    create_hashtable 8 [
      ("fun", FUN);
      ("cmp", CMP);
    ]

    let incr_linenum lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
          pos_lnum = pos.pos_lnum + 1;
          pos_bol = pos.pos_cnum;
        }
}

let quote = '\''
let digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let id = ['a'-'z' 'A'-'Z']['a'-'z'  'A'-'Z' '0'-'9' '_']*

rule token = parse
    | digit+ as inum
    { 
        let num = int_of_string inum in
        (* printf "integer: %s (%d)\n" inum num; *)
        NUM num
    }
    | "0x" hex_digit+ as hnum
    {
        let num = Scanf.sscanf hnum "0x%x" (fun x->x)  in
        (* printf "hex integer: %s (%d)\n" hnum num; *)
        NUM num
    }
    | quote ['\x1f'-'\x7f'] quote as qc
    {
        let c = String.get qc 1 in
        let num = Char.code c in
        NUM num
    }
    | id ':' as label
    {
        let label = String.sub label 0 (String.length label - 1) in
        (* printf "label: %s\n" label;*)
        LABEL label
    } 
    | 'j' id as branch
    {
        let cond = String.sub branch 1 (String.length branch - 1) in
        (*let f x = Printf.printf "%s," x in
        let _ = List.map f Ast.branches in*)
        if List.exists (fun x -> x = cond) Ast.branches then
            BRANCH cond
        else
            ID branch
    }
    | '"' [^'"']* '"' as str
    {
        let s = String.sub str 1 (String.length str - 1) in
        STR s
    }
    | id as word
    { try
        let token = Hashtbl.find keyword_table word in
        (*printf "keyword: %s\n" word;*)
        token
      with Not_found ->
        (*printf "identifier: %s\n" word;*)
        ID word
    }
    | '$' { DOLLAR }
    | '@' { AT }
    | '^' { XOR }
    | '|' { OR }
    | '&' { AND }
    | '~' { NOT }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { MUL }
    | '/' { DIV }
    | '=' { EQ }
    | ',' { COMMA }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '{' { LCURLY }
    | '}' { RCURLY }
    | '[' { LBRACKET }
    | ']' { RBRACKET }
    | '!' { BANG }

    | '#' [^'\n']*   (* eat up one-line comments *)
    { 
        token lexbuf 
    }
    | ['\n'] (* eat newlines *)
    {
        incr_linenum lexbuf;
        token lexbuf
    }
    | [' ' '\t' ] (* eat up whitespace *)
    { 
        token lexbuf 
    }
    | _ as c
    { 
        printf "Unrecognized character: %c\n" c;
        token lexbuf 
    }
    | eof
    { 
        EOF
    }

(*
{
  let rec parse lexbuf =
     let token = token lexbuf in
     (* do nothing in this example *)
     parse lexbuf

  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    try parse lexbuf
    with End_of_file -> ()

  let _ = Printexc.print main ()
}
*)
