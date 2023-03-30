(* 
 * CS546: http://www.csd.uoc.gr/~hy546/
 *
 * Homework 2: A lambda calculus interpreter
 * =========================================
 *
 *)
 (* Lecturer: Polyvios Pratikakis *)
 

(*
 * A. Utility functions
 *)

(*
 * A1. Write a function that computes the free variables of an AST
 * (Hint: look in the slides)
 *)
val freevars : Ast.exp -> Ast.var list

(*
 * A2. Write a function that takes an AST (the first argument),
 * replaces all free occurrences of variable x (the second argument)
 * with variable y (the third argument), and returns the renamed AST.
 *
 * Note: you can alpha-rename any bound variables in sub-trees as
 * needed, as long as the meaning of the program is the same
 * (alpha-equivalent).
 *)
val rename: Ast.exp -> Ast.var -> Ast.var -> Ast.exp

(*
 * A3. Write a function that checks if a program is a value or not.
 *)
val isvalue: Ast.exp -> bool

(*
 * A4. Write a function that replaces every free occurrence of a
 * variable x (the second argument) in an AST e (the first argument)
 * with another AST e' (the third argument).  Use alpha renaming to
 * avoid capturing of free variables.
 *)
val subst: Ast.exp -> Ast.var -> Ast.exp -> Ast.exp

(*
 * B. The lazy small-step interpreter
 *
 * Write the step function for lazy (call by name) small-step
 * operational semantics, as shown in the slides.
 *
 * The step function takes an AST, takes a step, and returns the
 * resulting AST, up to alpha-renaming.  I.e. you can rename bound
 * variables when you need to:
 * (fun x . x) is equivalent to (fun y . y)
 * The function raises an exception Cannot_step(e) for any program e
 * that cannot take a step
 *)
exception Cannot_step of Ast.exp
val lazy_step : Ast.exp -> Ast.exp

(*
 * C. The big-step interpreter
 *
 * Write the evaluation function for eager (call by value) big-step
 * operational semantics, as shown in the slides.
 *
 * The eval function takes an AST, evaluates it, and returns the
 * resulting AST (which cannot run further, is in normal form), if it
 * terminates.  You don't need to worry about non-terminating programs
 * and stack-overflow in your interpreter.
 *)
val eager_eval : Ast.exp -> Ast.exp
