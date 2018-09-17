open Hw1;;

type algebraic_term = Var of string 
  | Fun of string * (algebraic_term list)

(* По списку уравнений вернуть одно уравнение *)
val system_to_equation: (algebraic_term * algebraic_term) list -> (algebraic_term * algebraic_term)

(* Применить подстановку к уравнению *)
val apply_substitution: (string * algebraic_term) list -> algebraic_term -> algebraic_term

(* Проверить решение *)
val check_solution: (string * algebraic_term) list -> (algebraic_term * algebraic_term) list -> bool

(* Решить систему; если решения нет -- вернуть None *)
val solve_system: (algebraic_term * algebraic_term) list -> (string * algebraic_term) list option

type simp_type =
  | S_Elem of string
  | S_Arrow of simp_type * simp_type

val infer_simp_type : lambda -> ((string * simp_type) list * simp_type) option

type hm_lambda =
  | HM_Var of string
  | HM_Abs of string * hm_lambda
  | HM_App of hm_lambda * hm_lambda
  | HM_Let of string * hm_lambda * hm_lambda 

type hm_type =
  | HM_Elem of string 
  | HM_Arrow of hm_type * hm_type 
  | HM_ForAll of string * hm_type

val algorithm_w : hm_lambda -> ((string * hm_type) list * hm_type) option