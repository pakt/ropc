%{

open Printf
open Lexing

open Ast

(* get meta / save meta *)
let get_meta n = 
    let p = Parsing.rhs_start_pos n in
    let lnum = p.pos_lnum in
    (* let col = p.pos_cnum - p.pos_bol in *)
    {lnum=lnum} 

let wrap node = 
    let meta = get_meta 1 in
    {n=node; m=meta}

let only_small l = 
    let p x = x>255 in
    try let _ = List.find p l in false with Not_found -> true

let unescape s =
  Scanf.sscanf ("\"" ^ s ^ "\"") "%S" (fun u -> u)

let explode s =
  let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) ['\x00']

let str_to_byte_list s = 
    let s = unescape s in
    let l = explode s in
    let l = List.map (fun c -> Char.code c) l in
    l

let assign_tab id l =
    if only_small l then
        let s = AssignTab(id, l) in 
        s
    else failwith "Only byte values (0-255) allowed in tabs"

%}

/* Ocamlyacc Declarations */
%token NEWLINE EOF
%token EQ COMMA BANG
%token LPAREN RPAREN 
%token LCURLY RCURLY
%token LBRACKET RBRACKET
%token <int> NUM
%token <string> LABEL
%token <string> BRANCH
%token DOLLAR AT 
%token PLUS MINUS MUL DIV XOR OR AND
%token NOT 
%token <string> STR
%token <string> ID
%token <float->float> FNCT

%token FUN CMP

%left MUL DIV
%left PLUS MINUS
%left XOR OR AND
%left NOT   /* negation -- unary minus */

%start input
%type <Ast.program'> input

/* Grammar follows */
%%
exp:    
    | NUM               { let e = Const($1) in e }
    | ID                { let e = Var($1) in e }
    | AT ID             { let e = Ref($2) in e } 
    | exp PLUS exp      { let e = BinOp($1, Add, $3) in e }
    | exp MINUS exp     { let e = BinOp($1, Sub, $3) in e }
    | exp MUL exp       { let e = BinOp($1, Mul, $3) in e }
    | exp DIV exp       { let e = BinOp($1, Div, $3) in e }
    | exp XOR exp       { let e = BinOp($1, Xor, $3) in e }
    | exp OR exp        { let e = BinOp($1, Or, $3) in e }
    | exp AND exp       { let e = BinOp($1, And, $3) in e }
    | LBRACKET ID RBRACKET { let e = ReadMem($2) in e }
    | NOT exp           { let e = UnOp(Not, $2)  in e}
    | MINUS exp         { let e = UnOp(Sub, $2)  in e}
    | LPAREN exp RPAREN { let e = $2 in e }
;

num_list:
    | num_list COMMA NUM     { $3::$1 }
    | NUM                    { [$1] }
;

stmt:
    | ID EQ STR                     {   let l = str_to_byte_list $3 in 
                                        let s = assign_tab $1 l in
                                        wrap s
                                    }
    | ID EQ LBRACKET num_list RBRACKET  { let s = assign_tab $1 (List.rev $4) in wrap s }
    | ID EQ exp                     { let s = Assign($1,$3) in wrap s }
    | DOLLAR ID EQ exp              { let s = DerefAssign($2,$4) in wrap s }
    | LABEL                         { let s = Label($1) in wrap s }
    | BRANCH ID                     { let cond = Ast.str_to_cond $1 in let s = Branch(cond, $2) in wrap s }
    | CMP exp COMMA exp             { let s = Cmp($2, $4) in wrap s }
    | LBRACKET ID RBRACKET EQ exp   { let s = WriteMem($2, $5) in wrap s }
    | ID exp_args                   { let s = Call($1, $2) in wrap s }
    | BANG ID exp_args              { let s = ExtCall($2, $3) in wrap s }
;

stmt_list:
    | stmt_list stmt    { $2::$1 }
    | stmt              { [$1] }
;

args_list:
    | args_list COMMA ID    { $3::$1 }
    | ID                    { [$1] }
    |                       { [] } 
;

exp_args_list:
    | exp_args_list COMMA exp   { $3::$1 }
    | exp                       { [$1] }
    |                           { [] } 
;

args: LPAREN args_list RPAREN           { Args(List.rev $2) };
exp_args: LPAREN exp_args_list RPAREN   { ExpArgs(List.rev $2) };

func_body:
    | LCURLY stmt_list RCURLY   { FunBody'(List.rev $2) };

func:
    | FUN ID args func_body     { let f = Fun'($2, $3, $4) in wrap f }
;

func_list:
    | func_list func    { $2::$1 }
    | func              { [$1] }
;

input: EOF          { Prog'([]) }
    | func_list     { Prog'(List.rev $1) }
;

%%

