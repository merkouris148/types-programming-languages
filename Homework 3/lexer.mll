(********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 2
 *)
 (* Lecturer: Polyvios Pratikakis *)
 

{
  open Parser
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }
  | ['0'-'9']+ as lxm   { INT(int_of_string lxm) }
  | "fun"               { LAMBDA }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "true"              { TRUE }
  | "false"             { FALSE }
  | "Bool"              { BOOL }
  | "Unit"              { TUNIT }
  | "()"                { UNIT }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '.'                 { DOT }
  | "->"                { ARROW }
  | ':'                 { COLON }
  | ';'                 { SEMICOLON }
  | ['a'-'z']+ as id    { IDENT (id) }
  | eof                 { EOF }
