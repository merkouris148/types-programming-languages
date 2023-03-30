(********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 2
 *)
 (* Lecturer: Polyvios Pratikakis *)
 

open Homework2

let parse_stdin () =
  let lexbuf = Lexing.from_channel stdin in
    Parser.main Lexer.token lexbuf

let ast_of_string s =
  let lexbuf = Lexing.from_string s in
    Parser.main Lexer.token lexbuf


let rec stepper f e =
  try stepper f (f e) with Cannot_step (e') -> e
(*
let _ =
  let x = parse_stdin () in
  lazy_step x
*)
