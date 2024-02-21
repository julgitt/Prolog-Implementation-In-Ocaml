%token<string> VAR SYM
%token <string> FILEPATH
%token<int> NUM
%token BR_OPN BR_CLS PARENTH_OPN PARENTH_CLS 
%token SQR_BR_OPN SQR_BR_CLS BAR
%token COMMA DOT RULE
%token PLUS MINUS ASTERISK SLASH IS
%token EOF

%type<Ast.program> program
%start program

%type<Ast.query> query
%start query

%{

open Ast

let desugar_list term_list =
    let desugar_elem term acc = 
        match term with
        | Var (_, _) -> term
        | _ -> Sym ("cons", [term; acc])
    in
    List.fold_right desugar_elem term_list (Sym ("nil", []))

let desugar_cons car cdr = Sym ("cons", [car; desugar_list cdr])

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
;

sub_sym
: MINUS { "-" }
;

mult_sym
: ASTERISK { "*" }
;

div_sym
: SLASH    { "/" }


/* ========================================================================= */

term
: term_add is_sym term_add { Sym($2, [ $1; $3 ]) }
| term_add { $1 }
;

term_add
: term_primary add_sym term_add   { Sym($2, [ $1; $3 ]) }
| term_subtract add_sym term_add  { Sym($2, [ $1; $3 ]) }
| term_subtract { $1 }
| term_primary  { $1 }
;

term_subtract
: term_primary sub_sym term_primary  { Sym($2, [ $1; $3 ]) }
| term_subtract sub_sym term_primary { Sym($2, [ $1; $3 ]) }
;

term_primary
: term_div { $1 }
| term_neg { $1 }
;

term_div
: term_neg div_sym term_neg { Sym($2, [ $1; $3 ]) }
| term_div div_sym term_neg { Sym($2, [ $1; $3 ]) }
| term_div mult_sym term_neg { Sym($2, [ $1; $3 ]) }
| term_mult { $1 }
;

term_mult
: term_neg mult_sym term_neg  { Sym($2, [ $1; $3 ]) }
| term_mult mult_sym term_neg { Sym($2, [ $1; $3 ]) }
;

term_neg
: sub_sym term_neg { Sym($1, [ $2 ]) }
| term_simple { $1 }
;


/* ========================================================================= */

term_simple
: PARENTH_OPN term PARENTH_CLS { ($2) }
| VAR                { Var ($1, None) }
| symbol             { Sym ($1, []) }
| NUM                { (Num  $1) }
| symbol PARENTH_OPN term_list PARENTH_CLS { Sym($1, $3) }
| prolog_list        { $1 }
;

/* ========================================================================= */

term_list
: term                 { [ $1 ]   }
| term COMMA term_list { $1 :: $3 }
;

/* ========================================================================= */

prolog_list
: SQR_BR_OPN SQR_BR_CLS    { Sym("nil", []) }
| SQR_BR_OPN term_list SQR_BR_CLS    { desugar_list $2 }
| SQR_BR_OPN head = term BAR tail = term_list SQR_BR_CLS    { desugar_cons head tail }  
;

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
