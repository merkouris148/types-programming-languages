(************************************************
 * Types and Programming Languages
 * Assignment 3
 * ---------------------------------------------
 * Author: 			Merkouris Papamichail
 * Academinc Id: 	csdp1299
 * email: 			merkourisp@csd.uoc.gr
 * Submitted in:	16/12/2022
 ************************************************)

(* User Defined Modules *)
open Ast


(*
 * A. Write a function that takes an environment and a variable name,
 * looks up the name in the environment and returns the corresponding
 * type.  Raise exception Type_error if there is no corresponding
 * binding in the given environment.
 *)

 
(*exception Type_error*)
(*val lookup: Ast.env -> Ast.var -> Ast.typ*)
exception Type_error
let rec lookup gamma x = match gamma, x with
	| (y, t) :: gamma_tail, x	when y = x	-> t
	| (y, t) :: gamma_tail, x	when y != x	-> (lookup gamma_tail x)
	| [], _									-> raise Type_error
	;;


(*
 * B. Write a function that takes an environment and an AST and
 * returns a type of the given term.  Raise exception Type_error if
 * there can be no type.
 *)
 
 (***********
  * Helpers *
  ***********)
let is_func t = match t with
	| TFun(_, _)	-> true
	| _				-> false


let func_fst t = match t with
	| TFun(t1, t2)	-> t1
	;;


let func_snd t = match t with
	| TFun(t1, t2)	-> t2
	;;


exception Debug
(*val typeof: Ast.env -> Ast.exp -> Ast.typ*)
let rec typeof gamma expr = match gamma, expr with
(*
 *				
 *	[T-Unit]	-----------------------------
 *					\Gamma |- () : Unit
 *)
	| gamma, EVal(VUnit)	-> TUnit


(*
 *				x : T \in Gamma
 *	[T-Var]	------------------------
 *				\Gamma |- x:T
 *)
	| gamma, EVar(v)		-> (lookup gamma v)


(*
 *				
 *	[T-True]	-----------------------------
 *					\Gamma |- true : Bool
 *)
	| gamma, EVal(VTrue)	-> TBool
 
 

(*
 *				
 *	[T-False]	-----------------------------
 *					\Gamma |- false : Bool
 *)
	| gamma, EVal(VFalse)	-> TBool



(*
 *						\Gamma, x : T_1 |- e_2 : T_2
 *	[T-Abs]		--------------------------------------------
 *					\Gamma |- λx : T_1. e_2 : T_1 --> T_2
 *)
	| gamma, EVal(VFun(x, t1, expr))
	-> 
		let t2 = (typeof ((x, t1)::gamma) expr) in
		TFun(t1, t2)


(*
 *						\Gamma |- e1 : T_1 --> T2
 *						\Gamma |- e2 : T1
 *	[T-App]		--------------------------------------------
 *						\Gamma |- e1 e2 : T2
 *)
	| gamma, EApp(e1, e2)
	when not (is_func (typeof gamma e1))
	-> raise Type_error
	
	| gamma, EApp(e1, e2)
	->
		let t1		= typeof gamma e2 in
		let t		= typeof gamma e1 in
		let t1_func	= (func_fst t) in
		let t2		= (func_snd t) in
		if t1_func <> t1 then raise Type_error
		else t2

(*
 *						\Gamma |- e1 : Bool
 *						\Gamma |- e2 : T
 *						\Gamma |- e3 : T
 *	[T-If]		--------------------------------------------
 *				\Gamma |- if e1 then e2 else e2 : T
 *)
	| gamma, EIf(e1, e2, e3)
	->
	let t1 = typeof gamma e1 in
	let t2 = typeof gamma e2 in
	let t3 = typeof gamma e3 in
	if t1 = TBool && t2 = t3 then t2
	else raise Type_error


(*
 *						\Gamma |- e1 : Unit
 *						\Gamma |- e2 : T
 *	[T-Seq]		--------------------------------------------
 *						\Gamma |- e1; e2 : T
 *)
	| gamma, ESeq(e1, e2)
	->
	let t1 = typeof gamma e1 in
	let t2 = typeof gamma e2 in
	if t1 != TUnit then raise Type_error
	else t2


(* Everything Else Raises a Type_error exception! *)
	|_				-> raise Type_error
	;;
