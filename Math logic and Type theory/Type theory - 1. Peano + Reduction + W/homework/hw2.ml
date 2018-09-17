open Hw1;;

module SS = Set.Make (String);;
module SM = Map.Make(String);;

type algebraic_term = Var of string 
  | Fun of string * (algebraic_term list)

let unique_name prefix
  = Stream.from (fun i -> Some (prefix ^ "" ^ string_of_int i));;

let unique_type = unique_name "type";;
let unique_var = unique_name "var";;

(* // UNIFY // *)

exception NoSolution of string;;

(* From interface *)
let system_to_equation system
  = let
      next_name = Stream.next unique_var
    in let (l, r)
      = List.split system
    in (Fun (next_name, l), Fun (next_name, r));;

(* From interface *)
let apply_substitution str term
  = let rec applier subst term
    = let (key, value) = subst
      in match term with
         | Var a -> if (key = a)
                    then value
                    else term
         | Fun (a, args) -> Fun (a, List.map (applier subst) args)
    in List.fold_right applier str term;;

(* From interface *)
let check_solution subst stm
  = let (l, r) 
    = system_to_equation stm
    in let sub 
      = apply_substitution subst
    in sub l = sub r;;

(* From interface *)
let rec solve_system sys
  = let rec contains var term
    = match term with
      | Var a -> a = var
      | Fun (a, args) -> List.exists (contains var) args
    in let eq_substitute (l, r)
      = match l with
        | Var a -> (a, r)
        | _ -> raise (NoSolution "Equation is not resolved")
    in let rec solver sys res
      = match sys with
        | [] -> []
        | _ when 
          SS.cardinal res = List.length sys -> sys
        | eq :: eqs ->
          match eq with
          | (l, r) when
            l = r -> solver eqs res
          | (Var a, r) -> if contains a r
                          then raise (NoSolution "Recursive entry of variable")
                          else 
            let nres 
              = SS.add a res
            in let subst 
              = apply_substitution [(a, r)]
            in let rest 
              = List.map (fun (f, s) -> (subst f, subst s)) eqs
            in solver (rest @ [eq]) nres
          | (l, Var a) -> solver (eqs @ [(Var a, l)]) res (* swap sides *)
          | (Fun (fa, argsa), Fun (fb, argsb)) -> 
            if fa = fb && List.length argsa = List.length argsb
            then let neqs 
                = List.combine argsa argsb
              in solver (eqs @ neqs) res
            else raise (NoSolution "Equations with not matching functions")
    in try
      let res_sys 
        = solver sys SS.empty
      in let solution 
        = List.map eq_substitute res_sys
      in Some solution
    with NoSolution _ -> None;;

(* // INFERENCE // *)

type simp_type =
  | S_Elem of string
  | S_Arrow of simp_type * simp_type

type hm_type =
  | HM_Elem of string 
  | HM_Arrow of hm_type * hm_type 
  | HM_ForAll of string * hm_type

type hm_lambda =
  | HM_Var of string
  | HM_Abs of string * hm_lambda
  | HM_App of hm_lambda * hm_lambda
  | HM_Let of string * hm_lambda * hm_lambda 

let rec to_term tp
  = match tp with
    | S_Elem a -> Var a
    | S_Arrow (a, b) -> Fun ("->", [(to_term a); (to_term b)]);;

let rec to_type term
  = match term with
    | Var a -> S_Elem a
    | Fun (f, [a; b]) when
      f = "->" -> S_Arrow (to_type a, to_type b)
    | _ -> failwith "Bad term (simple type not parsed)";;

(* From interface *)
let infer_simp_type lm
  = let eq_of_types (a, b) 
      = (to_term a, to_term b)
    in let next_type () 
      = S_Elem (Stream.next unique_type)
    in let map_type map tp 
      = SM.add tp (next_type ()) map
    in let types 
      = List.fold_left map_type SM.empty (free_vars lm)
    in let rec build_system lm types
      = match (lm : lambda) with
        | Var a -> ([], SM.find a types)
        | App (lma, lmb) -> 
          let (sysa, ta) 
            = build_system lma types
          in let (sysb, tb) 
            = build_system lmb types
          in let tp 
            = next_type () 
          in (sysa @ sysb @ [(ta, S_Arrow (tb, tp))], tp)
        | Abs (lma, lmb) ->
          let next_map 
            = map_type types lma
          in let (sys, t) 
            = build_system lmb next_map
          in (sys, S_Arrow (SM.find lma next_map, t))
    in let (sys, t) 
      = build_system lm types
    in match solve_system (List.map eq_of_types sys) with
       | Some solution ->
          let type_term 
            = apply_substitution solution (to_term t)
          in let mk_type (a, b) 
            = (a, to_type b)
          in let type_list 
            = List.map mk_type
          in Some (type_list solution, to_type type_term)
       | _ -> None;;

let rec to_hm_term tp
  = match tp with
    | HM_Elem a -> Var a
    | HM_Arrow (a, b) -> Fun ("->", [to_hm_term a; to_hm_term b])
    | _ -> failwith "Quantor Ɐ can't be turned to term";;

let rec to_hm_type term
  = match term with
    | Var a -> HM_Elem a
    | Fun (f, [a; b]) when
      f = "->" -> HM_Arrow (to_hm_type a, to_hm_type b)
    | _ -> failwith "Bad term (HM type not parsed)";;

let rec hm_free_types hm_tp
  = match hm_tp with
    | HM_Elem a -> SS.singleton a
    | HM_Arrow (a, b) -> SS.union (hm_free_types a) (hm_free_types b)
    | HM_ForAll (a, b) -> SS.remove a (hm_free_types b);;

let rec hm_free_vars hm_lm
  = match hm_lm with
    | HM_Var a -> SS.singleton a
    | HM_Abs (a, b) -> SS.remove a (hm_free_vars b)
    | HM_App (a, b) -> SS.union (hm_free_vars a) (hm_free_vars b)
    | HM_Let (a, b, c) -> SS.union (hm_free_vars b) (SS.remove a (hm_free_vars c));;

let rec hm_type_subst hm_tp sub
  = match hm_tp with
    | HM_Elem a -> if SM.mem a sub
                   then SM.find a sub
                   else hm_tp
    | HM_Arrow (a, b) -> HM_Arrow (hm_type_subst a sub, hm_type_subst b sub)
    | HM_ForAll (a, b) -> HM_ForAll (a, hm_type_subst b (SM.remove a sub));;

let hm_env_subst env sub
  = SM.map (fun a -> hm_type_subst a sub) env;;

let hm_generalize env hm_tp
  = let union_types _ v
      = SS.union (hm_free_types v)
    in let env_free_types
      = SM.fold union_types env SS.empty
    in let hm_free_types
      = hm_free_types hm_tp
    in let next_A_vars
      = SS.diff hm_free_types env_free_types
    in let join_A var hm_tp
      = HM_ForAll (var, hm_tp)
    in SS.fold join_A next_A_vars hm_tp;;

let algorithm_w hm_lm
  = let uniq_var ()
      = Stream.next unique_var
    in let sub_composition suba subb
      = let tmp_sub
          = SM.map (fun a -> hm_type_subst a suba) subb
        in let merger key va vb
          = match (va, vb) with
            | (None, None) -> None
            | (Some v, None) -> Some v
            | (_, Some v) -> Some v
        in SM.merge merger suba tmp_sub
    in let rec propogate hm_tp
      = match hm_tp with
        | HM_ForAll (a, b) ->
          let sub
            = SM.singleton a (HM_Elem (uniq_var ()))
          in hm_type_subst (propogate b) sub
        | _ -> hm_tp
    in let rec solver env hm_lm
      = match hm_lm with
        | HM_Var a -> if SM.mem a env
                      then (SM.empty, propogate (SM.find a env))
                      else raise (NoSolution "Free variable")
        | HM_Abs (a, b) -> 
          let next_tp 
            = HM_Elem (uniq_var ())
          in let env
            = SM.add a next_tp (SM.remove a env)
          in let (f, s) 
            = solver env b
          in (f, HM_Arrow (hm_type_subst next_tp f, s))
        | HM_App (a, b) ->
          (let (fa, sa) 
            = solver env a
          in let (fb, sb)
            = solver (hm_env_subst env fa) b
          in let next_tp
            = HM_Elem (uniq_var ())
          in let (l, r)
            = (hm_type_subst sa fb, HM_Arrow (sb, next_tp))
          in let eq
            = (to_hm_term l, to_hm_term r)
          in match solve_system [eq] with
             | None -> raise (NoSolution "No solution")
             | Some solution ->
                let add_subst (str, term)
                  = SM.add str (to_hm_type term)
                in let v
                  = List.fold_right add_subst solution SM.empty
                in let unifier
                  = sub_composition v (sub_composition fa fb)
                in (unifier, hm_type_subst next_tp unifier))
        | HM_Let (a, b, c) ->
          let (fa, sa)
            = solver env b
          in let type_of_a
            = hm_generalize (hm_env_subst env fa) sa
          in let env
            = hm_env_subst (SM.remove a env) fa
          in let env
            = SM.add a type_of_a env
          in let (fb, sb)
            = solver env c
          in (sub_composition fb fa, sb)
    in let mark_unique a
      = SM.add a (HM_Elem (uniq_var ()))
    in let env 
      = SS.fold mark_unique (hm_free_vars hm_lm) SM.empty
    in try
      let (unifier, hm_tp)
        = solver env hm_lm
      in Some (SM.bindings unifier, hm_tp)
    with NoSolution _ -> None;;