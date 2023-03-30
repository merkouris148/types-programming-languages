/********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 2
 */

%{
open Ast
%}

%token EOF
%token <int> INT
%token <string> IDENT
%token LPAREN RPAREN
%token LAMBDA DOT
%right DOT
%start main
%type <Ast.exp> main
%type <Ast.exp> expr
%type <exp> app_expr

%%

main:
    expr EOF                      { $1 }
;

expr:
    LAMBDA IDENT DOT expr         { EVal(VFun($2, $4)) }
  | app_expr                      { $1 }
;

app_expr:
    aexp                          { $1 }
  | app_expr aexp                 { EApp($1, $2) }
;

aexp:
    IDENT                         { EVar $1 }
  | LPAREN expr RPAREN            { $2 }
;
