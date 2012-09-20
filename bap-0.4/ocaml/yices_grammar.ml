type token =
  | VAR of (string)
  | VAL of (int64)
  | SEMICOLON
  | LBRACKET
  | RBRACKET
  | EQUAL
  | MODEL
  | ASSERT
  | DASHES
  | INVALID
  | VALID
  | COMMA
  | PERIOD
  | EOF

open Parsing;;
let yytransl_const = [|
  259 (* SEMICOLON *);
  260 (* LBRACKET *);
  261 (* RBRACKET *);
  262 (* EQUAL *);
  263 (* MODEL *);
  264 (* ASSERT *);
  265 (* DASHES *);
  266 (* INVALID *);
  267 (* VALID *);
  268 (* COMMA *);
  269 (* PERIOD *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* VAR *);
  258 (* VAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\005\000\002\000\004\000\000\000"

let yylen = "\002\000\
\005\000\002\000\000\000\002\000\005\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\006\000\007\000\008\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\001\000\000\000\005\000"

let yydgoto = "\002\000\
\005\000\006\000\011\000\007\000\012\000"

let yysindex = "\001\000\
\246\254\000\000\000\000\000\000\000\000\252\254\004\000\001\255\
\000\000\000\255\254\254\001\255\007\255\009\000\000\000\008\255\
\000\000\006\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\255\
\000\000\000\000\000\000\003\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\002\000\000\000\000\000"

let yytablesize = 14
let yytable = "\003\000\
\004\000\001\000\008\000\009\000\010\000\013\000\014\000\016\000\
\017\000\018\000\019\000\003\000\000\000\015\000"

let yycheck = "\010\001\
\011\001\001\000\007\001\000\000\004\001\006\001\009\001\001\001\
\000\000\002\001\005\001\009\001\255\255\012\000"

let yynames_const = "\
  SEMICOLON\000\
  LBRACKET\000\
  RBRACKET\000\
  EQUAL\000\
  MODEL\000\
  ASSERT\000\
  DASHES\000\
  INVALID\000\
  VALID\000\
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
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'goodresult) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assertions) in
    Obj.repr(
# 24 "yices_grammar.mly"
                                         ( Some(_3) )
# 103 "yices_grammar.ml"
               : (string * int64) list option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'badresult) in
    Obj.repr(
# 25 "yices_grammar.mly"
                ( None )
# 110 "yices_grammar.ml"
               : (string * int64) list option))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "yices_grammar.mly"
              ( [] )
# 116 "yices_grammar.ml"
               : 'assertions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'assertion) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assertions) in
    Obj.repr(
# 30 "yices_grammar.mly"
                         ( _1 :: _2 )
# 124 "yices_grammar.ml"
               : 'assertions))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int64) in
    Obj.repr(
# 34 "yices_grammar.mly"
                                  ( (_3, _4) )
# 132 "yices_grammar.ml"
               : 'assertion))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "yices_grammar.mly"
          ( )
# 138 "yices_grammar.ml"
               : 'goodresult))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "yices_grammar.mly"
        ( )
# 144 "yices_grammar.ml"
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
