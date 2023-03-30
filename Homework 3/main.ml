(********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 2
 *)
 (* Lecturer: Polyvios Pratikakis *)
 
 
open Ast
open Homework3

let parse_stdin () =
  let lexbuf = Lexing.from_channel stdin in
    Parser.main Lexer.token lexbuf

let ast_of_string s =
  let lexbuf = Lexing.from_string s in
    Parser.main Lexer.token lexbuf

let rec string_of_typ = function
  | TBool -> "Bool"
  | TUnit -> "Unit"
  | TFun(TBool, t2) -> "Bool -> " ^ (string_of_typ t2)
  | TFun(TUnit, t2) -> "Unit -> " ^ (string_of_typ t2)
  | TFun(t1, t2) -> "(" ^ string_of_typ t1 ^ ") -> " ^ (string_of_typ t2)

let rec string_of_ast = function
  | EVar v -> v
  | EVal(VTrue) -> "true"
  | EVal(VFalse) -> "false"
  | EVal(VUnit) -> "()"
  | EVal(VFun(x, t, e)) -> 
      "(fun " ^ x
      ^ " : " ^ (string_of_typ t)
      ^ " . " ^ (string_of_ast e)
      ^ ")"
  | EApp(e1, e2) -> "(" ^ (string_of_ast e1) ^ " " ^ (string_of_ast e2) ^ ")"
  | ESeq(e1, e2) -> "(" ^ (string_of_ast e1) ^ "; " ^ (string_of_ast e2) ^ ")"
  | EIf(e1, e2, e3) ->
      "if " ^ (string_of_ast e1)
      ^ " then " ^ (string_of_ast e2) ^ " else " ^ (string_of_ast e3)
;;


let term = parse_stdin () in
print_string ("term: " ^ (string_of_ast term) ^ "\n");
let t = typeof [] term in
print_string ("type: " ^ (string_of_typ t) ^ "\n")

