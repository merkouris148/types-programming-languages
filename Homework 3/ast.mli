(********************************************************************
 * CS546 - http://www.csd.uoc.gr/~hy546/
 *
 * This file is part of homework 3
 *)
 (* Lecturer: Polyvios Pratikakis *)
 
(*
 * Types are either boolean, or number, or a function of an argument
 * type to a result type.
 *) 
type typ =               (* Types, which can be: *)
  | TBool               (* Boolean type (the type of true and false) *)
  | TUnit               (* Unit *)
  | TFun of typ * typ   (* Function type, e.g bool -> bool.
                         * The function type constructor constructs a
                         * function type given the type of the
                         * argument and the type of the result.
                         * For example bool -> bool is
                         *  TFun (TBool, TBool)
                         *)

(* a variable is a string identifier *)
type var = string

(*
 * Values are lambda terms, boolean constants and unit.
 *
 *    VFun("x", t, e)
 *
 * is the term "lambda x:t . e", a function with formal argument "x",
 * a variable with type t, and body e, an expression.
 *)
type value =
  | VFun of var * typ * exp
  | VTrue
  | VFalse
  | VUnit

(*
 * Expressions are either values (functions) or variables, or the
 * application of an expression to another expression.
 *
 * The types for value and expression are mutually recursive (note the
 * "and" keyword in the type definition of exp), because they refer to
 * each other: a value (which is a function) contains the body of the
 * function (which is an expression), and value expressions EVal(v)
 * contain a value v.
 *)
and exp =                   (* Expression type.  Expressions are: *)
  | EVar of var             (* Variables, e.g. EVar("x") *)
  | EVal of value           (* Values, which are functions (see 
                             * the definition of value), e.g.
                             * (lambda x:Bool . x) is
                             * EVal(VFun("x", TBool, EVar("x")))
                             *)

  | EApp of exp * exp       (* The application of an expression to
                             * another expression.  Here the first
                             * expression is the function applied, and
                             * the second is the argument.  For
                             * example (e1 e2) is EApp(e1, e2).
                             *)
  | EIf of exp * exp * exp  (* An "if" expression.  The first
                             * sub-expression is the condition, the
                             * second is the "then" branch, and the
                             * third is the "else" branch.
                             *)
  | ESeq of exp * exp       (* A sequencing expression "e1; e2". The
                             * first sub-expression is evaluated to
                             * (), and then the second is evaluated
                             * and its value is the result of the
                             * ESeq.
                             *)

(*
 * A typing environment Gamma.  Is a list of pairs, where the first
 * element of each pair is a variable name, and the second element is
 * its type in the environment.
 *)
type env = (var * typ) list

(*
 * A. Utility functions
 *)

(*
 * A function that computes the free variables of an AST
 *)
val freevars : exp -> var list

(*
 * Create and return a fresh variable, given a string of the old
 * variable.
 *)
val next : var -> var

(*
 * alpha-renaming:
 * A function that takes an AST (the first argument), replaces all
 * free occurrences of variable x (the second argument) with variable
 * y (the third argument), and returns the alpha-renamed AST.
 *)
val rename: exp -> var -> var -> exp

(*
 * A function that checks whether a program is a value or not.
 *)
val isvalue: exp -> bool

(*
 * Capture-avoiding substitution: replaces every free occurrence of a
 * variable x (the second argument) in an AST e (the first argument)
 * with another AST e' (the third argument).
 *)
val subst: exp -> var -> exp -> exp

(*
 * The interpreter raises this exception when it has reached a value.
 *)
exception Finished

(*
 * The interpreter raises this exception when it gets stuck (i.e.
 * cannot continue without having reached a value.
 *)
exception Stuck

(*
 * A call-by-value, small-step interpreter.  Raises Finished with
 * applied to a value, or Stuck when it cannot take a step.
 *)
val eager_step : exp -> exp

(*
 * A function that takes a small-step interpreter and a term, and runs
 * to completion, or forever, or gets stuck.  Raises Stuck if it
 * cannot take a step, or returns the resulting value when the program
 * terminates.
 *)
val stepper : (exp -> exp) -> exp -> exp
