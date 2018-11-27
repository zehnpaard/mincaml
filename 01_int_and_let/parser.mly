%{
open Syntax
let addtyp x = (x, Type.gentyp ())
%}

%token <int> INT
%token MINUS
%token <Id.t> IDENT
%token LET
%token IN
%token LPAREN
%token RPAREN
%token EOF

%right prec_let
%left MINUS

%type <Syntax.t> exp
%start exp

%%

simple_exp:
  | LPAREN exp RPAREN
    { $2 }
  | INT
    { Int($1) }
  | IDENT
    { Var($1) }

exp:
  | simple_exp
    { $1 }
  | exp MINUS exp
    { Sub($1, $3) }
  | LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
  | error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
          (Parsing.symbol_start ())
        (Parsing.symbol_end ())) }
