type token =
  | VAR of (string)
  | VAL of (int64)
  | SEMICOLON
  | LBRACKET
  | RBRACKET
  | EQUAL
  | ASSERT
  | INVALID
  | VALID
  | SAT
  | DASH
  | MODEL
  | COMMA
  | PERIOD
  | EOF

open Parsing;;
# 2 "cvc3_grammar.mly"
  let cond_append l = function
    | None -> l
    | Some(e) -> e ::l
# 24 "cvc3_grammar.ml"
let yytransl_const = [|
  259 (* SEMICOLON *);
  260 (* LBRACKET *);
  261 (* RBRACKET *);
  262 (* EQUAL *);
  263 (* ASSERT *);
  264 (* INVALID *);
  265 (* VALID *);
  266 (* SAT *);
  267 (* DASH *);
  268 (* MODEL *);
  269 (* COMMA *);
  270 (* PERIOD *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* VAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\005\000\005\000\002\000\002\000\
\004\000\000\000"

let yylen = "\002\000\
\003\000\002\000\000\000\002\000\007\000\007\000\002\000\002\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\007\000\009\000\008\000\000\000\000\000\000\000\002\000\000\000\
\001\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\005\000"

let yydgoto = "\002\000\
\006\000\007\000\013\000\008\000\014\000"

let yysindex = "\004\000\
\248\254\000\000\249\254\250\254\253\254\000\000\255\254\010\000\
\000\000\000\000\000\000\007\255\012\000\255\254\000\000\012\255\
\000\000\000\000\008\255\002\255\010\255\011\255\014\255\015\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\006\000\000\000\000\000"

let yytablesize = 20
let yytable = "\003\000\
\004\000\005\000\021\000\022\000\001\000\012\000\009\000\010\000\
\011\000\015\000\016\000\017\000\019\000\020\000\023\000\024\000\
\025\000\026\000\003\000\018\000"

let yycheck = "\008\001\
\009\001\010\001\001\001\002\001\001\000\007\001\014\001\014\001\
\012\001\000\000\004\001\000\000\001\001\006\001\005\001\005\001\
\003\001\003\001\000\000\014\000"

let yynames_const = "\
  SEMICOLON\000\
  LBRACKET\000\
  RBRACKET\000\
  EQUAL\000\
  ASSERT\000\
  INVALID\000\
  VALID\000\
  SAT\000\
  DASH\000\
  MODEL\000\
  COMMA\000\
  PERIOD\000\
  EOF\000\
  "

let yynames_block = "\
  VAR\000\
  VAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'goodresult) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assertions) in
    Obj.repr(
# 29 "cvc3_grammar.mly"
                            ( Some(_2) )
# 118 "cvc3_grammar.ml"
               : (string * int64) list option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'badresult) in
    Obj.repr(
# 30 "cvc3_grammar.mly"
                ( None )
# 125 "cvc3_grammar.ml"
               : (string * int64) list option))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "cvc3_grammar.mly"
              ( [] )
# 131 "cvc3_grammar.ml"
               : 'assertions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'assertion) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assertions) in
    Obj.repr(
# 35 "cvc3_grammar.mly"
                       ( cond_append _2 _1 )
# 139 "cvc3_grammar.ml"
               : 'assertions))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : int64) in
    Obj.repr(
# 39 "cvc3_grammar.mly"
                                                   ( Some(_3, _5) )
# 147 "cvc3_grammar.ml"
               : 'assertion))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 40 "cvc3_grammar.mly"
                                                   ( None )
# 155 "cvc3_grammar.ml"
               : 'assertion))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "cvc3_grammar.mly"
                 ( )
# 161 "cvc3_grammar.ml"
               : 'goodresult))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "cvc3_grammar.mly"
            ( )
# 167 "cvc3_grammar.ml"
               : 'goodresult))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "cvc3_grammar.mly"
               ( )
# 173 "cvc3_grammar.ml"
               : 'badresult))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (string * int64) list option)
