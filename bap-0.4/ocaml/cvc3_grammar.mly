%{
  let cond_append l = function
    | None -> l
    | Some(e) -> e ::l
%}

%token <string> VAR
%token <int64> VAL

%token SEMICOLON
%token LBRACKET RBRACKET
%token EQUAL
%token ASSERT
%token INVALID
%token VALID
%token SAT
%token DASH
%token MODEL
%token COMMA
%token PERIOD
%token EOF

%start main
%type <(string * int64) list option> main

%%

  main:
  goodresult assertions EOF { Some($2) }
| badresult EOF { None }
  ;

  assertions:
  /* empty */ { [] }
| assertion assertions { cond_append $2 $1 }
  ;

  assertion:
  ASSERT LBRACKET VAR EQUAL VAL RBRACKET SEMICOLON { Some($3, $5) } /* BV */
| ASSERT LBRACKET VAR EQUAL VAR RBRACKET SEMICOLON { None } /* Memory */
  ;

  goodresult:
  INVALID PERIOD { }
| SAT MODEL { }
  ;

badresult:
  VALID PERIOD { }
;
