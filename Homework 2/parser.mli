type token =
  | EOF
  | INT of (int)
  | IDENT of (string)
  | LPAREN
  | RPAREN
  | LAMBDA
  | DOT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
