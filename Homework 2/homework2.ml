(************************************************
 * Types and Programming Languages
 * Assignment 2
 * ---------------------------------------------
 * Author: 			Merkouris Papamichail
 * Academinc Id: 	csdp1299
 * email: 			merkourisp@csd.uoc.gr
 * Submitted in:	12/11/2022
 ************************************************)

open Ast 


(*********************
  * Helper Functions *
  ********************)

let rec list_member elem lls = match elem, lls with
	| _, []					-> false
	| l, k::ls when l = k	-> true
	| l, k::ls when k != l	-> list_member elem ls
	;;


(* val list_minus: int list -> int list -> int list *)
let rec list_minus lls kks = match lls, kks with
	| [], _										-> []
	| l::ls, kks when not (list_member l kks)	-> l::(list_minus ls kks)
	| l::ls, kks when list_member l kks			-> list_minus ls kks
	;;



(**************************
 * λ-calculus Definitions *
 **************************)

(* Free Variables of a Term *)
(* val freevars : Ast.exp -> Ast.var list *)
let rec freevars expr = match expr with
	| EVar(x)			-> [x]
	| EVal(VFun(x, e))	-> list_minus (freevars e) [x]
	| EApp(e1, e2)		-> (freevars e1) @ (freevars e2)
;;


(* generate fresh variables *)
(* ********************************************************
 * Input:
 *			1) a set of variables (as a list)
 *			2) a counter i
 *
 * Output:
 *			A variable of the form "vj", such that j
 *			is the smallest number j >= i, s.t.
 *			"vj" \notin variables.
 *
 * Note:
 *			We can use this function to generate fresh
 *			variables for a-renaming.
 ******************************************************** *)
let rec fresh_variable variables i = match variables, i with
	(*
	 * the variable v_i is unused, i.e.
	 * v_i \notin variables	
	*)
	| variables, i
		when
			not (
				list_member 
					("v"^(string_of_int i))
					variables
			)
		-> ("v"^(string_of_int i))
	(*
	 * the variable v_i is used, i.e.
	 * v_i \in variables	
	*)
	| variables, i
		when
			(
				list_member 
					("v"^(string_of_int i))
					variables
			)
		-> fresh_variable variables (i + 1)
;;

let fresh_variable variables = fresh_variable variables 0;;


(* Substitute Operation *)
(* ********************************************************
 * Input:
 *			1) a λ-term expr1
 *			2) a variable x
 *			3) a λ-term expr2
 *
 * Output:
 *			expr1[x := expr2], i.e.
 *			we replace all free occurrences of x, in expr1,
 *			with expr2.
 * ****************************************************** *)
(* val subst: Ast.exp -> Ast.var -> Ast.exp -> Ast.exp *)
let rec subst expr1 x expr2 = match expr1, x, expr2 with
	(* expr1 is a variable v, and v = x *)
	| EVar(v), x, expr2 when v = x
	-> expr2
	
	(* expr1 is a variable v, and v != x *)
	| EVar(v), x, expr2 when v <> x
	-> EVar(v)
	
	(* application expr1 = (e1 e2) *)
	| EApp(e1, e2), x, expr2
	-> EApp(subst e1 x expr2, subst e2 x expr2)
	
	(*
	 * Functions 1: we have a term of the form:
	 *	λv. e, when v = x.
	 * => Then we stop the substitution
	*)
	| EVal(VFun(v, e)), x, expr2 when v = x
	-> EVal(VFun(v, e))
	
	(*
	 * Functions 2: we have a term of the form:
	 *	λy. e,
	 * when
	 * (y \notin FV(expr2)) or  // caption avoiding
	 * (x \notin FV(e)			// (see bellow)
	 * => Then proceed to the inner term
	*)
	| EVal(VFun(y, e)), x, expr2
	when
		y <> x	 								&&
		(
			not (list_member y (freevars expr2)) 	||
			not (list_member x (freevars e))
		)
	-> EVal(VFun(y, subst e x expr2))
	
	(*
	 * Functions 3: we have a term of the form:
	 *	λy. e,
	 * when
	 * y \in FV(expr2) and	// critical point, we could capture unwanted variables
	 * x \in FV(e)			// since x is free in e, then the substitution will
	 *							substitute x with y, then we could have an unwanted
	 *							capture
	 * => We should do a a-renaming with y, e.g. convert y to z
	*)
	| EVal(VFun(y, e)), x, expr2
	when
		y <> x	 							&&
		(list_member y (freevars expr2)) 	&&
		(list_member x (freevars e))
	->
		let variables = (freevars e) @ (freevars expr2) in
		let z = fresh_variable variables				in
		let a_renamed = subst e y (EVar(z))				in
		EVal(VFun(z, subst a_renamed x expr2))
;;


(* Rename the free occurances of a variable *)
(* we can define rename as a special case of substitution *)
let rename expr1 x y = subst expr1 x (EVar(y));;


(* Is Value aka is of the form λx. e *)
let isvalue expr = match expr with
	| EVal(VFun(_, _))	-> true
	| _					-> false
;;



(*******************
 * Lazy Evaluation *
 *******************)
 
(* Lazy Evaluation implemented by
 * executing the leftmost β-reduction *)
(* val lazy_step : Ast.exp -> Ast.exp *)
exception Cannot_step of Ast.exp
let rec lazy_step expr = match expr with
	| EApp(EVal(VFun(x, e)), expr2)		-> subst e x expr2
	| EApp(e1, e2)						-> EApp(lazy_step e1, e2)
	| expr								-> raise (Cannot_step expr)
;;






(********************
 * Eager Evaluation *
 ********************)
 
(* Eagar Step *)
let rec eager_step expr = match expr with
	| EApp(EVal(VFun(x, e)), expr2)
	->
		let expr_2_prime = eager_step expr2 in
		subst e x expr_2_prime
	| EApp(e1, e2)						-> EApp(eager_step e1, eager_step e2)
	| EVal(VFun(x, e))					-> EVal(VFun(x, e))
	| EVar(x)							-> EVar(x)
;;
 
 
(* 0 in Scott numerals: λx.λy. x *)
let lambda_zero = EVal(VFun("x", EVal(VFun("y", EVar("x")))));;


(* The recursive definition of eager_eavl:
 * we stop computation when we reach a fixed point.*)
let rec eager_eval expr_old expr_new = match expr_old, expr_new with
	(* we have reached a fixed point *)
	| expr_old, expr_new when expr_old = expr_new
	-> expr_old
	
	(* we can make an eager step *)
	| expr_old, expr_new when expr_old != expr_new
	-> 
		let expr_new_new = eager_step expr_new in
		eager_eval expr_new expr_new_new
;;


(* val eager_eval : Ast.exp -> Ast.exp *)
let eager_eval expr = eager_eval lambda_zero expr;;
