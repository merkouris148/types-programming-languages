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
  | TBool               (* Boolean type (the type of true and false *)
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
let rec freevars ast =
  match ast with
    EVar x -> [x]
  | EVal (VFun(x, t, e)) -> List.filter (fun y -> x<>y) (freevars e)
  | EVal bv -> []
  | ESeq (e1, e2)
  | EApp (e1, e2) -> List.rev_append (freevars e1) (freevars e2)
  | EIf (e1, e2, e3) ->
      let fv1 = (List.rev_append (freevars e1) (freevars e2)) in
      List.rev_append fv1 (freevars e3)

(*
 * Create and return a fresh variable, given a string of the old
 * variable.
 *)
let next =
  let counter = ref 0 in
  fun str ->
    let r = !counter in
    incr counter;
    str^(string_of_int r)

(*
 * alpha-renaming:
 * A function that takes an AST (the first argument), replaces all
 * free occurrences of variable x (the second argument) with variable
 * y (the third argument), and returns the alpha-renamed AST.
 *)

let rec rename ast x y =
  match ast with
    EVar z when z = x -> EVar y
  | EVar z -> EVar z
  | EVal (VFun(z, _, _)) when z = x -> ast
  | EVal (VFun(z, t, e)) when z = y -> 
      let newz = next z in
      let e' = rename e z newz in
      EVal (VFun(newz, t, rename e' x y))
  | EVal (VFun(z, t, e)) -> EVal (VFun(z, t, rename e x y))
  | EVal bv -> EVal bv
  | EApp (e1, e2) -> EApp(rename e1 x y, rename e2 x y)
  | ESeq (e1, e2) -> ESeq(rename e1 x y, rename e2 x y)
  | EIf (e1, e2, e3) ->
      EIf(rename e1 x y, rename e2 x y, rename e3 x y)

(*
 * A function that checks whether a program is a value or not.
 *)
let isvalue ast =
  match ast with
    EVal _ -> true
  | _ -> false

(*
 * Capture-avoiding substitution: replaces every free occurrence of a
 * variable x (the second argument) in an AST e (the first argument)
 * with another AST e' (the third argument).
 *)
let subst e1 x e2 =
  let fv = freevars e2 in
  let rec mysubst e1 =
    match e1 with
      | EVar y when y = x -> e2
      | EVar y -> EVar y
      | EApp (f, a) -> EApp(mysubst f, mysubst a)
      | ESeq (e1, e2) -> EApp(mysubst e1, mysubst e2)
      | EVal(VFun(y, _, _)) when x = y -> e1
      | EVal(VFun(y, t, e1)) when List.mem y fv ->
          let y' = next y in
          let e1' = rename e1 y y' in
          mysubst (EVal(VFun(y', t, e1')))
      | EVal(VFun(y, t, e1)) -> EVal(VFun(y, t, mysubst e1))
      | EVal bv -> EVal bv
      | EIf(e1, e2, e3) -> EIf(mysubst e1, mysubst e2, mysubst e3)
  in mysubst e1


(*
 * This exception is thrown whenever the interpreter cannot take a
 * step because the program is a value
 *)
exception Finished

(*
 * The interpreter throws this exception when it cannot take a step
 * and the program is not a value, i.e., when it is stuck
 *)
exception Stuck

(*
 * A call-by-value, small-step interpreter
 * Throws Finished when called on a value, or Stuck when it cannot
 * take a step
 *)

let rec eager_step e =
  if isvalue e then raise Finished else
  match e with
  | EApp(EVal(VFun(x, t, e)), v) when isvalue v -> subst e x v
  | EApp(EVal v, e2) -> EApp(EVal v, eager_step e2)
  | EApp(e1, e2) -> EApp(eager_step e1, e2)
  | EIf(EVal VTrue, e2, e3) -> e2
  | EIf(EVal VFalse, e2, e3) -> e3
  | EIf(e1, e2, e3) -> EIf(eager_step e1, e2, e3)
  | ESeq(EVal(VUnit), e2) -> e2
  | ESeq(e1, e2) -> ESeq(eager_step e1, e2)
  | _ -> raise Stuck

let rec stepper f e =
  try stepper f (f e) with Finished -> e
