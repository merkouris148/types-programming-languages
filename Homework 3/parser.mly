/********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 2
 */
 /* Lecturer: Polyvios Pratikakis */
 

%{
open Ast
%}

%token EOF
%token <int> INT
%token <string> IDENT
%token UNIT TUNIT SEMICOLON
%token LPAREN RPAREN
%token LAMBDA DOT ARROW COLON BOOL
%token IF THEN ELSE TRUE FALSE
%token LET IN EQUAL
%right DOT ARROW
%start main
%type <Ast.exp> main
%type <Ast.exp> expr
%type <exp> app_expr
%type <typ> typ

%%

main:
    expr EOF                        { $1 }
;

typ:
    typ ARROW typ                   { TFun($1, $3) }
  | BOOL                            { TBool }
  | TUNIT                           { TUnit }
  | LPAREN typ RPAREN               { $2 }
;

expr:
    LAMBDA IDENT COLON typ DOT expr { EVal(VFun($2, $4, $6)) }
  | app_expr                        { $1 }
  | seq_expr                        { $1 }
  | IF expr THEN expr ELSE expr     { EIf($2, $4, $6) }
;

seq_expr:
    aexp                            { $1 }
  | expr SEMICOLON expr             { ESeq($1, $3) }
;

app_expr:
    aexp                            { $1 }
  | app_expr aexp                   { EApp($1, $2) }
;

aexp:
    IDENT                           { EVar $1 }
  | TRUE                            { EVal(VTrue) }
  | FALSE                           { EVal(VFalse) }
  | UNIT                            { EVal(VUnit) }
  | LPAREN expr RPAREN              { $2 }
;
