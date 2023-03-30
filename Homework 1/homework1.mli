(* Homework 1, CS546, UoC. *)
(* Lecturer: Polyvios Pratikakis *)

(* You should submit a homework1.ml file that implements this
 * interface file, and can be compiled with:
 * $ ocamlc -c homework1.mli homework1.ml
 *)


(************************************************************************
 * A: Lists and recursion
 * 
 * A.1: Write an OCaml function that takes a list of integers and a
 * threshold n, and splits the input list into two, one with all numbers
 * less than or equal to n, and the other with all numbers greater than
 * n.  It should return a pair (tuple of two) with the (less, more) lists.
 *)
val split_n : int list -> int -> (int list * int list)


(*
 * A.2: Write an Ocaml function that takes two lists of integers and
 * returns a list of the numbers that occur in both lists.
 *)
val find_common : int list -> int list -> int list

(*
 * A.3: Write an Ocaml function that takes two lists of integers and
 * returns a list of the numbers that occur in the first, but not the
 * second.
 *)
val list_minus: int list -> int list -> int list

(*
 * A.4: Write an OCaml function that takes three arguments: another
 * function of two arguments, and two lists of equal size.  You should
 * apply the input function on each i-th element of both lists and
 * return a list of the results.
 *)
val find_filter : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(*
 * A.5: Write an OCaml function that takes three arguments: another
 * function of two arguments, a list, and an integer n.  You should
 * apply the input function on every n-th element of the input list
 * and return a list of the results.
 *)
val map_nth : ('a -> 'b) -> 'a list -> int -> 'b list


(************************************************************************
 * B: N-base arithmetic (where N >= 2)
 * Assume that a list of ints contains the digits of a number in N-based
 * representation.  For example, for binary arithmetic it would contain
 * 1s and 0s, with the head of the list being the least significant bit,
 * and the last element being the most significant bit, with 4 being
 * the list [0;0;1].
 *)

(*
 * Write OCaml functions to do the following:
 * 
 * B.1: Given an integer n for the base, and an int list, check whether
 * the list is an n-base representation of a number.
 *)
val check_nbase : int -> int list -> bool

(*
 * B.2: Given an integer n for the base, and an integer a, return a list
 * of the n-base representation of the integer n.
 *)
val nbase_of_int : int -> int -> int list

(*
 * B.3: Given an integer n for the base, and a list with an n-base
 * represenation, return an integer with the represented number.
 *)
val int_of_nbase : int -> int list -> int

(*
 * B.4: Write a function of three arguments: an integer n for the
 * base, and two lists with an n-base represenation.  Subtract the
 * second from the first, and return a list with the n-base
 * representation of the difference.
 *)
val subtract_nbase: int -> int list -> int list -> int list

(*
 * B.5: Write a multiply function that takes three arguments:
 * an integer n for the base and two lists of integers representing
 * n-base numbers.  It should multiply the numbers and return an
 * integer list of the n-base representation of the product.
 *)
val multiply_nbase: int -> int list -> int list -> int list



(************************************************************************
 * C: Boolean syntax trees
 * Assume an abstract syntax tree of simple boolean formulas given by
 * the following type.  Remember you have to redefine the type in the
 * implementation file.
 *)

type bool_ast =
    True
  | False
  | And of bool_ast * bool_ast
  | Or of bool_ast * bool_ast
  | Not of bool_ast

(*
 * C.1: Write an OCaml function that takes a syntax tree and evaluates
 * it to true or false.
 *)
val eval_ast : bool_ast -> bool

(*
 * C.2: Write an OCaml function that takes a syntax tree and checks if
 * it contains any implications (A implies B is equivalent to not A or
 * B).
 *)
val has_implication : bool_ast -> bool

(*
 * C.3: Write an OCaml function that takes a syntax tree and
 * simplifies all double negations, returning a simpler syntax tree.
 *)
val remove_notnot : bool_ast -> bool_ast

(*
 * C.4: Write an OCaml function that takes another function and a
 * sytax tree, and returns a list of all the subtrees for which the
 * first argument returns true.
 *)
val select_subtrees : (bool_ast -> bool) -> bool_ast -> bool_ast list

(*
 * C.5: Write an OCaml function that takes a syntax tree and checks if
 * it contains any identities (an identity on A is the term "A and *
 * A").
 *)
val has_identity: bool_ast -> bool

(*
 * C.6: Write an OCaml function that takes a syntax tree and
 * rewrites all negations of products (Not (A and B)) to sums of
 * negations ((Not A) or (Not B)), returning the resulting tree.
 *)
val rewrite_demorgan: bool_ast -> bool_ast


 
