(* PEANO *)

type peano
  = Z
  | S of peano;;

let rec peano_of_int i
  = match i with
    | 0 -> Z
    | i -> S (peano_of_int (i - 1));;

let rec int_of_peano i
  = match i with
    | Z    -> 0
    | S ii -> (int_of_peano ii) + 1;;

let inc i
  = S i;;

let dec i
  = match i with
    | Z   -> Z
    | S x -> x;;

let rec add x y
  = match (x, y) with
    | (x, Z)   -> x
    | (Z, y)   -> y
    | (S x, y) -> S (add x y);;

let rec sub x y 
  = match (x, y) with
    | (x, Z)     -> x
    | (Z, y)     -> Z
    | (S x, S y) -> sub x y;;

let rec mul x y
  = match (x, y) with
    | (Z, _)   -> Z
    | (_, Z)   -> Z
    | (S x, y) -> add y (mul x y);;

let rec power x y
  = match (x, y) with
    | (_, Z)   -> S Z
    | (Z, _)   -> Z
    | (x, S y) -> mul x (power x y);;

(* LISTS *)

let rev lst
  = let rec rotor tmp
    = function
      | []    -> tmp
      | x::xs -> rotor (x::tmp) xs
  in rotor [] lst;;

let rec merge lst1 lst2
  = match (lst1, lst2) with
    | ([], y)                          -> y
    | (x, [])                          -> x
    | ((x::xs as l), (y::ys as r)) -> 
      if x < y
        then x :: (merge xs r)
        else y :: (merge l ys);;

let rec split i lst
  = match (i, lst) with
    | (0, x)     -> ([], x)
    | (i, [])    -> (lst, [])
    | (i, x::xs) ->
      let res 
        = split (i - 1) xs
      in 
        match res with
        | (l, r) -> (x::l, r);;

let rec merge_sort lst
  = match lst with
    | []  -> []
    | [x] -> [x]
    | x ->
      let (l, r)
        = split (List.length x / 2) x
      in merge (merge_sort l) (merge_sort r);;

(* LAMBDAS *)

open Genlex;;
open Stream;;

type lambda
  = Var of string
  | Abs of string * lambda
  | App of lambda * lambda;;

let rec string_of_lambda l
  = match l with
    | Var a -> a
    | Abs (a, l) -> "\\" ^ a ^ "." ^ string_of_lambda l
    | App (Abs (_, _) as abs, Var b) -> "(" ^ string_of_lambda (abs) ^ ") " ^ b
    | App (Abs (_, _) as abs, lb) -> "(" ^ string_of_lambda (abs) ^ ") (" ^ string_of_lambda lb ^ ")"
    | App (l, Var a) -> string_of_lambda l ^ " " ^ a
    | App (la, lb) -> string_of_lambda la ^ " (" ^ string_of_lambda lb ^ ")";;

let lambda_of_string str
  = let 
      str = str ^ ";"
    in let
      tokens = make_lexer [
        "\\"; ".";
        "("; ")";
        ";"
      ] (of_string str)
    in let 
      token_to_string t
        = match t with
          | Kwd k    -> k
          | Ident i  -> i
          | Int i    -> string_of_int i
          | String s -> s
          | Float f  -> string_of_float f
          | Char c   -> Char.escaped c
    in let
      is_parenthesis ()
        = if (next tokens <> Kwd ")")
            then failwith "No closing parenthesis"
    in let
      is_abstraction ()
        = if (next tokens <> Kwd ".")
          then failwith "Dot was expected as abstraction sing"
    in let

      rec parse ()
        = match next tokens with
          | Kwd "("  -> parse_parentheses ()
          | Kwd "\\" -> parse_abstraction ()
          | Ident a  -> parse_variable a
          | s        -> failwith ("Unexpected sign " ^ (token_to_string s))
      
      and parse_parentheses ()
        = let 
            l = parse ()
          in
            is_parenthesis ();
            is_application l
      
      and parse_abstraction ()
        = match next tokens with
          | Ident a ->
            is_abstraction ();
            let 
              l = parse ()
            in
              is_application (Abs (a, l))
          | s       -> failwith ("Unexpected sign " ^ (token_to_string s))

      and parse_variable a
        = is_application (Var a)

      and parse_application l token
        = match token with
          | Kwd ")"  -> l
          | Kwd ";"  -> l
          | Kwd "\\" -> App (l, parse ())
          | Kwd "("  ->
            let 
              _ = (next tokens) and ll = parse ()
            in
              is_parenthesis ();
              is_application (App (l, ll))
          | Ident a  ->
            let
              _ = next tokens
            in
              is_application (App (l, Var a))
          | s        -> failwith ("Unexpected sign " ^ (token_to_string s))

      and is_application l
        = match peek tokens with
          | None       -> failwith "Unexpected end of string"
          | Some token -> parse_application l token
    in parse ();;

(* MORE LAMBDAS *)

module SS = Set.Make (String);;
module SM = Map.Make (String);;

let rec free_var_set l
  = match l with
    | Var a        -> SS.singleton a
    | App (la, lb) -> SS.union (free_var_set la) (free_var_set lb)
    | Abs (a, l)   -> SS.remove a (free_var_set l);;

let free_vars l
  = SS.elements (free_var_set l);;

let rec has_free key l
  = match l with
    | Var a        -> a = key
    | App (la, lb) -> (has_free key la) || (has_free key lb)
    | Abs (a, l)   ->
      if (a = key)
        then false
        else has_free key l;;

let substitute src l key
  = let
      src_free_vars = free_var_set src
    in let
      no_key_to_replace l = not (has_free key l)
    in let
      not_bounding l = not (SS.mem l src_free_vars)
    in let
      rec substitutor l
        = match l with
          | Var a -> if (a = key)
                      then src
                      else l
          | App (la, lb) -> App (substitutor la, substitutor lb)
          | Abs (a, l) -> if (no_key_to_replace l)
                          then l
                          else if (not_bounding a)
                                then Abs (a, substitutor l)
                                else failwith ("not free for substitution")
    in substitutor l;;

let free_to_subst src l key
  = try
      let _ = substitute src l key
      in true
    with _ -> false;;

let rec is_normal_form l
  = match l with
    | Var _ -> true
    | Abs (a, l) -> is_normal_form l
    | App (Abs (a, la), lb) -> not (free_to_subst lb la a)
    | App (la, lb) -> is_normal_form la && is_normal_form lb;;

let unique_name 
  = Stream.from (fun i -> Some ("var_" ^ string_of_int i))

let is_alpha_equivalent la lb
  = let
      rec equivalentor la lb
        = match (la, lb) with
          | (Var a, Var b) -> a = b
          | (App (laa, lab), App (lba, lbb)) -> 
              (equivalentor laa lba && equivalentor lba lbb)
          | (Abs (a, la), Abs (b, lb)) ->
              let
                temp = Var (Stream.next unique_name)
              in equivalentor (substitute temp la a) (substitute temp lb b)
          | _ -> false
    in equivalentor la lb;;

(*
let is_alpha_equivalent_soft la lb
  = let
      check_vars a b mpa mpb
        = if (StringMap.mem a mpa) && (StringMap.mem b mpb)
              && (StringMap.find a mpa = b) && (StringMap.find b mpb = a)
          then true
          else if (not (StringMap.mem a mpa)) && (not (StringMap b mpb) && a = b)
                then true
                else false
    in let
      rec equivalentor la lb 
*)

type lambda_t
  = Var_t of string
  | Abs_t of (string * lambda_t ref)
  | App_t of (lambda_t ref * lambda_t ref);;

let rec lambda_t_of_lambda l
  = match l with
    | Var a -> ref (Var_t a)
    | App (la, lb) -> ref (App_t (lambda_t_of_lambda la, lambda_t_of_lambda lb))
    | Abs (a, l) -> ref (Abs_t (a, lambda_t_of_lambda l));;

let rec lambda_of_lambda_t l
  = match !l with
    | Var_t a -> Var a
    | App_t (la, lb) -> App (lambda_of_lambda_t la, lambda_of_lambda_t lb)
    | Abs_t (a, l) -> Abs (a, lambda_of_lambda_t l);;

let rec to_alpha_eq l mp 
  = match l with
    | Var a -> if SM.mem a mp 
                then Var (SM.find a mp) 
                else l
    | App (la, lb) -> App (to_alpha_eq la mp, to_alpha_eq lb mp)
    | Abs (a, la) ->
        let 
          temp = Stream.next unique_name 
        in 
          Abs (temp, to_alpha_eq la (SM.add a temp mp));;
    
let normal_beta_reduction_impl lr 
  = let 
      rec try_to_subst term lr var 
        = match !lr with
          | Var_t a -> if a = var 
                        then lr := !term
          | App_t (la, lb) -> 
            try_to_subst term la var; 
            try_to_subst term lb var
          | Abs_t (a, l) -> if a <> var 
                            then try_to_subst term l var
    in let 
      rec reduction_impl lr 
        = let app_case a b 
          = match !a with
            | Abs_t (aa, al) ->
              let 
                fresh_var = Stream.next unique_name 
              in
                lr := !(lambda_t_of_lambda 
                          (to_alpha_eq (lambda_of_lambda_t al) 
                          (SM.singleton aa fresh_var))
                        );
                try_to_subst b lr fresh_var;
                Some lr
            | _ ->
              match reduction_impl a with
              | Some x -> Some lr
              | None ->
                match reduction_impl b with
                | Some x -> Some lr
                | None -> None
    in
      match !lr with
      | Var_t a -> None
      | App_t (la, lb) -> app_case la lb
      | Abs_t (a, l) ->
        match reduction_impl l with
        | Some x -> Some lr
        | None -> None
    in reduction_impl lr;;

let normal_beta_reduction l
  = let 
      lr = lambda_t_of_lambda (to_alpha_eq l SM.empty) 
    in let 
      tmp = normal_beta_reduction_impl lr 
    in 
      match tmp with
      | Some x -> lambda_of_lambda_t x
      | None -> l;;

let reduce_to_normal_form l
  = let 
      lr = lambda_t_of_lambda (to_alpha_eq l SM.empty)
  in let 
    rec reducer r 
      = let 
          tmp = normal_beta_reduction_impl r 
        in
          match tmp with
          | Some x -> reducer r
          | None -> r
  in
    lambda_of_lambda_t (reducer lr);;
