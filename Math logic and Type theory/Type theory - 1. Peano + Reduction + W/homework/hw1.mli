type peano = Z | S of peano

val peano_of_int: int -> peano
val int_of_peano: peano -> int

val inc: peano -> peano
val dec: peano -> peano
val add: peano -> peano -> peano
val sub: peano -> peano -> peano
val mul: peano -> peano -> peano
val power: peano -> peano -> peano

val rev: 'a list -> 'a list
val merge: 'a list -> 'a list -> 'a list
val split: int -> 'a list -> 'a list * 'a list
val merge_sort: 'a list -> 'a list

type lambda = Var of string 
  | Abs of string * lambda 
  | App of lambda * lambda

val string_of_lambda: lambda -> string

(**
 *  <lambda> -> <expr> ' ' <abs> | <expr> | <abs>
 *  <abs> -> \<var>.<lambda>
 *  <expr> -> { <var> | (<lambda>) }+{' '}
 *)

val lambda_of_string: string -> lambda

(* Returns a set of free variables in `lambda` *)
val free_var_set         : lambda -> Set.Make (String).t
(* Returns a list of free variables in `lambda` *)
val free_vars            : lambda -> Set.Make (String).elt list
(* Returns true if `key` is free in `lambda` *)
val has_free             : string -> lambda -> bool
(* Returns true if `lambda` expression is in normal form *)
val is_alpha_equivalent  : lambda -> lambda -> bool
(* Returns true if `src` is free for substitution *)
(* in `dest` instead of `key`, false otherwise    *)
val free_to_subst        : lambda -> lambda -> string -> bool
(* Do one beta-reduction step for `lambda` using normal reduction order *)
val normal_beta_reduction: lambda -> lambda
(* Beta-reduce `lambda` to its normal form using normal reduction order *)
val reduce_to_normal_form: lambda -> lambda