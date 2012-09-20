
%token <string> VAR
%token <int64> VAL

%token SEMICOLON
%token LBRACKET RBRACKET
%token EQUAL
%token MODEL
%token ASSERT
%token DASHES
%token INVALID
%token VALID
%token COMMA
%token PERIOD
%token EOF

%start main
%type <(string * int64) list option> main

%%

main:
/* The result comes before or after the assertions depending on version. */
  goodresult MODEL assertions DASHES EOF { Some($3) }
| badresult EOF { None }
  ;

assertions:
  /* empty */ { [] }
  | assertion assertions { $1 :: $2 }
  ;

assertion:
  LBRACKET EQUAL VAR VAL RBRACKET { ($3, $4) }
  ;

goodresult:
  INVALID { }
;

badresult:
  VALID { }
;
