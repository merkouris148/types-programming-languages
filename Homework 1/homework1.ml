(************************************************
 * Types and Programming Languages
 * Assignment 1
 * ---------------------------------------------
 * Author: 			Merkouris Papamichail
 * Academinc Id: 	csdp1299
 * email: 			merkourisp@csd.uoc.gr
 * Submitted in:	1/11/2022
 ************************************************)



(**************
 * Exercise A *
 **************)

(* Question A.1 *)
let rec split_n_leq lls n = match lls, n with
    | [], _                 -> []
    | l::ls, n when l <= n  -> l::(split_n_leq ls n)
    | l::ls, n when l > n   -> split_n_leq ls n
    ;;

let rec split_n_g lls n =  match lls, n with
    | [], _                 -> []
    | l::ls, n when l <= n  -> split_n_g ls n
    | l::ls, n when l > n   -> l::(split_n_g ls n)
    ;;

    
(* val split_n : int list -> int -> (int list * int list) *)
(**********************************************************
 * Input: 	a) int list: lls
 *			b) int: n
 * Output	a) (int list: leq_list, int list: g_list),
 * 			where leq_list contains the elements of lls
 *			that are less or equal to n, while g_list
 *			contains the elements of lls that are grater
 *			than n.
 **********************************************************)
let split_n lls n = (split_n_leq lls n, split_n_g lls n);;
    

(* Question A.2 *)
let rec list_member elem lls = match elem, lls with
	| _, []					-> false
	| l, k::ls when l = k	-> true
	| l, k::ls when k != l	-> list_member elem ls
	;;


(* val split_n : int list -> int -> (int list * int list) *)
let rec find_common lls kks = match lls, kks with
	| [], _										-> []
	| l::ls, kks when (list_member l kks)		-> l::(find_common ls kks)
	| l::ls, kks when not (list_member l kks)	-> find_common ls kks
	;;

	
(* Question A.3 *)
(* val list_minus: int list -> int list -> int list *)
let rec list_minus lls kks = match lls, kks with
	| [], _										-> []
	| l::ls, kks when not (list_member l kks)	-> l::(list_minus ls kks)
	| l::ls, kks when list_member l kks			-> list_minus ls kks
	;;

	
(* Question A.4 *)
(* val find_filter : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
let rec find_filter func args1 args2 = match args1, args2 with
	| [], []				-> []
	| a1::rgs1, a2::rgs2	-> (func a1 a2)::(find_filter func rgs1 rgs2)
	;;

	
(* Question A.5 *)
let rec map_nth_aux func lls i n = match lls, i, n with
	| [], _, _					-> []
	| l::ls, 0, n				-> (func l)::(map_nth_aux func ls n n)
	| l::ls, i, n when i > 0	-> l::(map_nth_aux func ls (i-1) n)
	;;

(* val map_nth : ('a -> 'b) -> 'a list -> int -> 'b list *)
let map_nth func lls n = map_nth_aux func lls n n;;


(**************
 * Exercise B *
 **************)

(* Question B.1 *)
(* val check_nbase : int -> int list -> bool *)
let rec check_nbase base lls = match base, lls with
	| _,[]							-> true
	| base, l::ls when l >= base	-> false
	| base, l::ls when l < base		-> check_nbase base ls
	;;

	
(* Question B.2 *)
let rec reverse lls = match lls with
	| []	-> []
	| l::ls	-> (reverse ls) @ [l]
	;;


let rec nbase_of_int_aux num base = match num, base with
	| 0, _					-> []
	| num, base				-> (num mod base)::(nbase_of_int_aux (num / base) base)
	;;


(* val nbase_of_int : int -> int -> int list *)
let nbase_of_int num base = (reverse (nbase_of_int_aux num base));;


(* Question B.3 *)
let rec power a n = match a, n with
	| a, 0				-> 1
	| a, n when n > 0	-> a * (power a n-1)
	;;


let rec int_of_nbase_aux lls base i = match lls, base with
	| [], _				-> []
	| l::ls, base		-> l * (power base i) :: (int_of_nbase_aux ls base (i+1))
	;;

let rec sum lls = match lls with
	| []		-> 0
	| l::ls		-> l + (sum ls)
	;;


(* val int_of_nbase : int -> int list -> int *)
let int_of_nbase base lls = sum (int_of_nbase_aux (reverse lls) base 0);;


(* Question B.4 *)
let rec add_nbase_aux lls kks curry base = match lls, kks, curry, base with
	(* curry == 1*)
	(* we leave the right hand operand kks as is and we try to eliminate the curry *)
	| [], kks, 1, _								-> add_nbase_aux [1] kks 0 base
	| l::ls, kks, 1, base when base > l + 1		-> add_nbase_aux ((l + 1)::ls) kks 0 base
	| l::ls, kks, 1, base when base <= l + 1	-> add_nbase_aux (((l + 1) mod base)::(add_nbase_aux ls [1] 0 base)) kks 0 base
	(* curry == 0 *)
	(* case 1: the addition does not create curry *)
	| [], kks, 0, _								-> kks
	| lls, [], 0, _								-> lls
	| l::ls, k::ks, 0, base when base > l + k	-> (l + k)::(add_nbase_aux ls ks 0 base)
	(* case 2: the addition does create curry *)
	| l::ls, k::ks, 0, base when base <= l + k	-> ((l + k) mod base)::(add_nbase_aux ls ks 1 base)
	;;
	
	
	
let add_nbase base lls kks = (reverse (add_nbase_aux (reverse lls) (reverse kks) 0 base));;


(* There is a spelling error *)
let rec subtrack_nbase_aux lls kks curry base = match lls, kks, curry, base with
	(* curry == 1*)
	(* we leave the left hand operand lls as is and we try to eliminate the curry *)
	| lls, [], 1, base							-> subtrack_nbase_aux lls [1] 0 base
	| lls, k::ks, 1, base when base > k + 1		-> subtrack_nbase_aux lls ((k + 1)::ks) 0 base
	| lls, k::ks, 1, base when base <= k + 1	-> subtrack_nbase_aux lls (((k + 1) mod base)::(add_nbase base [1] ks)) 0 base
	(* curry == 0 *)
	(* case 1: the addition does not create curry *)
	| [], k::ks, 0, _							-> failwith "Negative Result!"
	| lls, [], 0, _								-> lls
	| l::ls, k::ks, 0, base when l >= k			-> (l - k)::(subtrack_nbase_aux ls ks 0 base)
	(* case 2: the addition does create curry *)
	| l::ls, k::ks, 0, base when l < k			-> ((base + l - k) mod base)::(subtrack_nbase_aux ls ks 1 base)
	;;


(* val subtract_nbase: int -> int list -> int list -> int list *)
let subtract_nbase base lls kks = (reverse (subtrack_nbase_aux (reverse lls) (reverse kks) 0 base));;


(* Question B.5 *)
let rec add_nbase_list_aux lls sum base = match lls,sum, base with
	| [], sum, _		-> sum
	| l::ls, sum, base	-> add_nbase_list_aux ls (add_nbase base l sum) base
	;;

let add_nbase_list lls base = (add_nbase_list_aux lls [] base);;


let rec shift_left num n = match num, n with
	| num, 0		-> num
	| num, n		-> shift_left (num @ [0]) (n - 1)
	;;

(* "Scalar" Product *)
let rec scalar_product_aux lls n curry base = match lls, n, curry, base with
	| [], _, 0, _						-> []
	| [], _, curry, _	when curry > 0	-> [curry]
	| l::ls, n, curry, base				-> ( ( l*n + curry ) mod base)::(scalar_product_aux ls n (( l*n + curry ) / base) base)
	;;

let scalar_product lls n base = reverse (scalar_product_aux (reverse lls) n 0 base);;


let rec multiply_nbase_aux lls kks i base = match lls, kks, i, base with
	| _, [], _, _			-> []
	| lls, k::ks, i, base	-> (shift_left (scalar_product lls k base) i)::(multiply_nbase_aux lls ks (i + 1) base)
	;;


(* val multiply_nbase: int -> int list -> int list -> int list *)
let multiply_nbase base lls kks = add_nbase_list (multiply_nbase_aux lls (reverse kks) 0 base) base;;



(**************
 * Exercise C *
 **************)
 
type bool_ast =
	| True
	| False
	| And of bool_ast * bool_ast
	| Or of bool_ast * bool_ast
	| Not of bool_ast
	;;

	
(* Question C.1 *)
(* val eval_ast : bool_ast -> bool *)
let rec eval_ast term = match term with
	| True			-> true
	| False			-> false
	| Not(t)		-> not (eval_ast t)
	| And(t1, t2)	-> (eval_ast t1) && (eval_ast t2)
	| Or(t1, t2)	-> (eval_ast t1) || (eval_ast t2)
	;;


(* Question C.2 *)
(* val has_implication : bool_ast -> bool *)
let rec has_implication term = match term with
	| Or(Not(t1), t2)		-> true
	| And(t1, t2)			-> (has_implication t1) || (has_implication t2)
	| Or(t1, t2)			-> (has_implication t1) || (has_implication t2)
	| Not(t)				-> (has_implication t)
	| _						-> false
	;;


(* Question C.3 *)
(* val remove_notnot : bool_ast -> bool_ast *)
let rec remove_notnot term = match term with
	| Not(Not(t))	-> remove_notnot t
	| And(t1, t2)	-> And( remove_notnot t1, remove_notnot t2 )
	| Or(t1, t2)	-> Or( remove_notnot t1, remove_notnot t2 )
	| Not(t)		-> Not( remove_notnot t )
	| True			-> True
	| False			-> False
	;;


(* Question C.4 *)
(* val select_subtrees : (bool_ast -> bool) -> bool_ast -> bool_ast list *)
let rec select_subtrees predicate term = match predicate, term with
	| predicate, And(t1, t2) when (predicate (And(t1, t2)))		-> (And(t1, t2))::( (select_subtrees predicate t1) @ (select_subtrees predicate t2) )
	| predicate, And(t1, t2) when not (predicate (And(t1, t2)))	-> (select_subtrees predicate t1) @ (select_subtrees predicate t2)
	| predicate, Or(t1, t2) when (predicate (Or(t1, t2)))		-> (Or(t1, t2))::( (select_subtrees predicate t1) @ (select_subtrees predicate t2) )
	| predicate, Or(t1, t2) when not (predicate (Or(t1, t2)))	-> (select_subtrees predicate t1) @ (select_subtrees predicate t2)
	| predicate, Not(t) when (predicate (Not(t)))				-> (Not(t))::(select_subtrees predicate t)
	| predicate, Not(t) when not (predicate (Not(t)))			-> (select_subtrees predicate t)
	| predicate, True when (predicate True)						-> [True]
	| predicate, True when not (predicate True)					-> []
	| predicate, False when (predicate False)					-> [False]
	| predicate, False when not (predicate False)				-> []
	;;

	
(* Question C.5 *)
(* val has_identity: bool_ast -> bool *)
let rec has_identity term = match term with
	| And(t1, t2) when t1 = t2	-> true
	| And(t1, t2)				-> (has_implication t1) || (has_implication t2)
	| Or(t1, t2)				-> (has_implication t1) || (has_implication t2)
	| Not(t)					-> (has_implication t)
	| _							-> false
	;;


(* Question C.6 *)
(* val rewrite_demorgan: bool_ast -> bool_ast *)
let rec rewrite_demorgan term = match term with
	| (Not (And(t1,t2)))		-> And( Not( rewrite_demorgan t1 ) , Not( rewrite_demorgan t2 ) )
	| And(t1, t2)				-> And( rewrite_demorgan t1, rewrite_demorgan t2)
	| Or(t1, t2)				-> Or( rewrite_demorgan t1, rewrite_demorgan t2 )
	| Not(t)					-> Not( rewrite_demorgan t )
	| True						-> True
	| False						-> False
	;;
