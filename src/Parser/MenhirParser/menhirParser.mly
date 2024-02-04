%token<string> VAR SYM
%token <string> FILEPATH
%token<int> NUM
%token BR_OPN BR_CLS PARENTH_OPN PARENTH_CLS
%token COMMA DOT RULE
%token PLUS MINUS ASTERISK SLASH IS
%token EOF

%type<Ast.program> program
%start program

%type<Ast.query> query
%start query

%{

open Ast

%}

%%

symbol
: SYM { $1 }
;


is_sym
: IS { "is" }
;

add_sym
: PLUS  { "+" }
| MINUS { "-" }
;

mult_sym
: ASTERISK { "*" }
| SLASH    { "/" }
;


/* ========================================================================= */

term
: term_add is_sym term_add { Sym($2, [ $1; $3 ]) }
| term_add { $1 }
;

term_add
: term_mult add_sym term_add { Sym($2, [ $1; $3 ]) }
| term_mult { $1 }
;

term_mult
: term_neg mult_sym term_mult { Sym($2, [ $1; $3 ]) }
| term_neg { $1 }
;

term_neg
: add_sym term_neg { Sym($1, [ $2 ]) }
| term_simple { $1 }
;


/* ========================================================================= */

term_simple
: PARENTH_OPN term PARENTH_CLS { ($2) }
| VAR                { Var ($1, ref None) }
| symbol             { Sym ($1, []) }
| NUM                { (Num  $1) }
| symbol PARENTH_OPN term_list PARENTH_CLS { Sym($1, $3) }
;

/* ========================================================================= */

term_list
: term                 { [ $1 ]   }
| term COMMA term_list { $1 :: $3 }
;

/* ========================================================================= */

clause
: term DOT                { Fact $1      }
| term RULE term_list DOT { Rule($1, $3) }
;

clause_list_rev
: /* empty */            { []       }
| clause_list_rev clause { $2 :: $1 }
;

clause_list
: clause_list_rev { List.rev $1 }
;

/* ========================================================================= */

program
: clause_list EOF { $1 }
;

query
: term_list DOT {Query $1 }
| BR_OPN FILEPATH BR_CLS {Filepath $2}
;
