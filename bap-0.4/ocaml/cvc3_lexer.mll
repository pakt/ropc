{
  open Cvc3_grammar
  (* TODO: add support for memories *)
}

let digit = ['0'-'9''A'-'F''a'-'f']
let varname = ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9''_''['']']*
let newline = '\n'

rule token = parse
  | [' ' '\t' '\n']  { token lexbuf }
  | ['=']            { EQUAL }
  | [',']            { COMMA }
  | ['.']            { PERIOD }
  | [';']            { SEMICOLON }
  | ['(']            { LBRACKET }
  | [')']            { RBRACKET }
  | '%' | "Current"  { read_comment lexbuf }
  | "ASSERT"         { ASSERT }
  | "Invalid"        { INVALID }
  | "Valid"          { VALID }
  | "sat"            { SAT }
  | "MODEL"          { MODEL }
  | "-"              { DASH }
  | "0hex"           { read_num lexbuf }
  | "0bin"             { read_numbin lexbuf }
  | varname as var   { VAR var }
  | eof              { EOF }
  | _                { token lexbuf }

and read_num = parse
  | digit+ as n      { VAL(Int64.of_string ("0x"^n)) }
  | _                { token lexbuf }

and read_numbin = parse
  | digit+ as n      { VAL(Int64.of_string ("0b"^n)) }
  | _                { token lexbuf }

(* Ignore any line starting with % or Current *)
and read_comment = parse
  | newline          { token lexbuf }
  | _                { read_comment lexbuf }

