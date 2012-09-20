(* Lexer for our IR language *)
(* Author: David Brumley, Ivan Jager *)
(* $Id: lexer.mll 4973 2011-08-22 21:04:00Z edmcman $ *)

{
 open Grammar;;               (* open the grammar for the tokens *)
 open Lexing;;

 exception LexError of string

   (* if true, we keep single-line slashy-slashy comments *)
 let flag_keep_linecomments = ref true;;
 (* if true, we keep slashy star star slashy comments *)
 let flag_keep_blkcomments = ref true;;

 let track_line_numbers = ref true;;


 let get = Lexing.lexeme

   (* this function is present in ocaml 3.11, but not in Debian
      lenny's ocaml *)
 let new_line lexbuf = 
   let lcp = lexbuf.lex_curr_p in 
     lexbuf.lex_curr_p <- {lcp with
			     pos_lnum = lcp.pos_lnum + 1;
			     pos_bol = lcp.pos_cnum;
			  }
 ;;

 let incr_linenum lexbuf =
   if !track_line_numbers then 
     new_line lexbuf


(* quoted string handling code taken from:
   http://caml.inria.fr/pub/ml-archives/caml-list/2005/10/d22d128ac2fd06df780201ccb4b49ead.en.html
*)

 let char_for_backslash = function   
   | 'a' -> '\007'
   | 'v' -> '\011'
   | 'f' -> '\012'
   | 'n' -> '\n'
   | 't' -> '\t'
   | 'b' -> '\b'
   | 'r' -> '\r'
   | c   -> c
       

  

 let string_buff = Buffer.create 256
 let reset_string_buffer () = Buffer.clear string_buff  
 let store_string_char c = Buffer.add_char string_buff c
 let store_string s = Buffer.add_string string_buff s
 let get_stored_string () = Buffer.contents string_buff


 let in_comment = ref 0;;
 let comment_start () =
   assert(!in_comment = 0);
   in_comment := 1;
   reset_string_buffer()  
 let enter_comment () = 
   in_comment := !in_comment + 1
 let exit_comment () = 
   in_comment := !in_comment - 1
 let in_comment () = !in_comment > 0;;


 let eof ()  = 
   if (in_comment ())  then
     raise (LexError "Unterminated comment")
   else
     EOF


 let lexbuf_error_to_string  lexbuf = 
   let pos = lexbuf.Lexing.lex_curr_p in 
   let linepos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in 
   let lexstr = (get lexbuf) in 
     Printf.sprintf "line %u pos %u: %s" pos.Lexing.pos_lnum
       linepos lexstr

}



let bs_escapes = [ '\032' - '\255' ]
let alpha = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let hexinteger = ('0'('x' | 'X'))hexdigit+
let id = alpha(digit|alpha)*
let nl = ('\r' | '\n' | "\r\n")
let ws = [' ''\t']
let fname = ('"')[^'\r' '\n']*('"')[^'\r' '\n']* nl
let singlecomment = "//"[^'\n' '\n']*nl 

(***********************************************************************)
(* Parse the source-code \ comments \ cpp *)
(***********************************************************************)

rule token = parse
  | eof          { eof() }
  | (ws)+        { token lexbuf }
  | nl           { incr_linenum lexbuf; token lexbuf }

(* keywords *)
  | "cjmp"       { CJMP }
  | "jmp"        { JMP } 
  | "halt"       { HALT }
  | "assert"     { ASSERT }
  | "special"    { SPECIAL }
  | "unknown"    { UNKNOWN }
  | "with"       { WITH }      
  | "let"        { LET }
  | "in"         { IN }
  | "label"      { LABEL } 
  | "addr"       { ADDR }
  | "extract"    { EXTRACT }
  | "concat"     { CONCAT } 
 
(* if then else expressions *)
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }  
(* sugar *)
  | "true"       { TRUE }
  | "false"      { FALSE }
  | "e_big"      { EBIG }
  | "e_little"   { ELITTLE }
(* id must come after all keywords *)
  | '{'          { LCURLY }
  | '}'          { RCURLY }
  | '['          { LSQUARE }
  | ']'          { RSQUARE }
  | '+'          { PLUS }
  | '-'          { MINUS }
  | '*'          { TIMES }
  | '/'          { DIVIDE }
  | "$/"         { SDIVIDE }
  | '%'          { MOD }
  | "$%"         { SMOD }
  | "<<"         { LSHIFT }
  | ">>"         { RSHIFT }
  | "$>>"        { ARSHIFT }
  | '&'          { AND }
  | '|'          { OR }
  | '^'          { XOR } 
  | "="          { EQUAL }
  | "=="         { EQUALEQUAL }
  | "<>"         { NEQ } 
  | '<'          { LT }
  | "<="         { LE } 
  | "$<"         { SLT } 
  | "$<="        { SLE } 
  | '>'          { GT }
  | ">="         { GE } 
  | "$>"         { SGT } 
  | "$>="        { SGE } 
  | '~'          { NOT }
  | ":="         { ASSIGN }
  | ':'          { COLON } 
  | ';'          { SEMI }
  | '('          { LPAREN }
  | ')'          { RPAREN }
  | ','          { COMMA } 
  | '@'          { AT }
  | '?'          { QUESTION }
  | '"'          { reset_string_buffer ();
	   	   scan_str lexbuf;
		   let s = get_stored_string () in
		      STRING(s)
	         }

  | "/*"         { comment_start();
		   blkcomment lexbuf;
		   if !flag_keep_blkcomments then 
		     COMMENT(get_stored_string())
		   else
		     token lexbuf 
		 }
  | singlecomment { if !flag_keep_linecomments then
		      let s = get lexbuf in 
			(* -3 to remove the newline *)
			COMMENT(String.sub s 2 ((String.length s) -3)) 
		    else
		      token lexbuf 
		  } 
  | "#"           { cpptoken lexbuf }
  | id           { ID(get lexbuf) }
  | digit+ | hexinteger { 
      try INT(Util.big_int_of_string (get lexbuf))
      with Failure "int_of_string" -> 
	raise(LexError "Error converting integer");
    }
  | _ as s    { raise(LexError("line "^
                               (string_of_int lexbuf.lex_curr_p.pos_lnum)
                               ^": Unrecognized char '"^Char.escaped s^"'")) }


(* probably not the best way to implement this, but the lack of block comments
   has been bugging me too much. --aij *)
and blkcomment = parse 

| "/*"         { store_string "/*"; enter_comment();  blkcomment lexbuf }
|  "*/"
    { if in_comment () && (exit_comment(); (*still_*)in_comment())
      then 
	(store_string "*/"; blkcomment lexbuf)
    }
| nl           { incr_linenum lexbuf; store_string_char '\n'; blkcomment lexbuf }
|  eof       { ignore(eof ()) } 
| _ as c         { store_string_char c; blkcomment lexbuf  }

and scan_str = parse
  | ['"']   {  () }
  | '\\'  (bs_escapes as c)
            { store_string_char (char_for_backslash c);
	      scan_str lexbuf 
	    }
  | eof      { raise(LexError "Unterminated string") }
  | _  as c   {  store_string_char c; scan_str lexbuf }
			   
and cpptoken = parse
  | digit+ { (* if !track_line_numbers then (
	       fileoffset := (Pervasives.int_of_string (get lexbuf));
	     );*)
             cpptoken lexbuf
           }
  | fname { 
            token lexbuf
          }
  | nl  { token lexbuf }
  | ws+   { cpptoken lexbuf}
  | eof { eof () }
  | _ { 
      raise(LexError "Unexpected cpp characters")
      }
