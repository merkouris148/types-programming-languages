type token =
  | EOF
  | INT of (int)
  | IDENT of (string)
  | UNIT
  | TUNIT
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LAMBDA
  | DOT
  | ARROW
  | COLON
  | BOOL
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | LET
  | IN
  | EQUAL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
