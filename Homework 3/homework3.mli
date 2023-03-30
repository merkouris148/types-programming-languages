(* 
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * Homework 3: A lambda calculus type checker
 * ==========================================
 *
 *)
 (* Lecturer: Polyvios Pratikakis *)
 

(*
 * A. Write a function that takes an environment and a variable name,
 * looks up the name in the environment and returns the corresponding
 * type.  Raise exception Type_error if there is no corresponding
 * binding in the given environment.
 *)

(*
 *				x : T \in Gamma
 *	[T-Var]	------------------------
 *				\Gamma |- x:T
 *)
 
exception Type_error
val lookup: Ast.env -> Ast.var -> Ast.typ

(*
 * B. Write a function that takes an environment and an AST and
 * returns a type of the given term.  Raise exception Type_error if
 * there can be no type.
 *)
val typeof: Ast.env -> Ast.exp -> Ast.typ
