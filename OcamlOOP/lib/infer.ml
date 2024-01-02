(** Copyright 2021-2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

module R : sig
  include Base.Monad.Infix

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left xs ~init ~f =
      Base.Map.fold xs ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TPrim _ -> false
    | TList t -> occurs_in v t
    | TTuple ts -> List.fold_left (fun ans t -> occurs_in v t || ans) false ts
  ;;

  let vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TPrim _ -> acc
      | TList t -> helper acc t
      | TTuple ts -> List.fold_left (fun acc t -> helper acc t) acc ts
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax
  open Base

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  (* проверяем, что k не входит в v и возвращем представление замены *)
  let mapping k v =
    if Type.occurs_in k v then fail @@ `Occurs_check (k, v) else return (k, v)
  ;;

  (* список из одной замены k |-> v *)
  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find k xs = Map.find xs k
  let remove xs k = Map.remove xs k

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find b s with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TList t -> tlist (helper t)
      | TTuple ts -> ttuple @@ List.map ts ~f:(fun t -> helper t)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TPrim _, TPrim _ -> fail (`Unification_failed (l, r))
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TList t1, TList t2 -> unify t1 t2
    | TTuple t1, TTuple t2 ->
      (match
         List.fold2 t1 t2 ~init:(return empty) ~f:(fun acc e1 e2 ->
           let* subs = acc in
           let s1 = apply subs e1 in
           let s2 = apply subs e2 in
           let* sub = unify s1 s2 in
           let* final_subs = compose subs sub in
           return final_subs)
       with
       | Ok subs -> subs
       | _ -> fail (`Unification_failed (ttuple t1, ttuple t2)))
    | _ -> fail (`Unification_failed (l, r))

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Base.Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2
  ;;

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

module VarSet = struct
  include VarSet

  let fold_left_m f set acc =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      set
      acc
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    (* переменная не обобщена, но встречается в выражении *)
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    (* не заменяем переменные, которые обобщены *)
    S (names, Subst.apply s2 ty)
  ;;

  let apply2 sub new_names (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (new_names, Subst.apply s2 ty)
  ;;
end

(*
   type t = (fresh, ty, Int.comparator_witness) Map.t

   let empty = Map.empty (module Int)
*)
module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  (* type t = (string * scheme) list *)

  let extend env (v, scheme) = Map.update env v ~f:(fun _ -> scheme)
  (* let extend e h = h :: e *)

  let replace_scheme env (v, new_scheme) =
    let new_env =
      List.fold_left env ~init:[] ~f:(fun env ((var, _) as h) ->
        if String.equal v var then (v, new_scheme) :: env else h :: env)
    in
    List.rev new_env
  ;;

  let rec extend1 (S (bs, ty) as scheme) acc =
    let open Ast in
    function
    | Pat_var (Id v) -> extend acc (v, scheme)
    | Pat_cons (_, _) -> acc (* ИСПРАВИТЬ !!! *)
    | Pat_tuple es ->
      (match ty with
       | TTuple ts ->
         let new_env =
           List.fold2 es ts ~init:acc ~f:(fun acc e t -> extend1 (S (bs, t)) acc e)
         in
         (match new_env with
          | Ok env -> env
          | _ -> acc)
       | _ -> acc)
    | _ -> acc
  ;;

  let empty = Map.empty (module String)
  (* let empty = [] *)

  let free_vars =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  (* let free_vars =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;*)

  let apply s env = Map.map env ~f:(Scheme.apply s)

  (* let apply s env = List.Assoc.map env ~f:(Scheme.apply s) *)

  let find xs name = Map.find xs name
  (* let find xs name = List.Assoc.find ~equal:String.equal xs name *)
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> tvar n

let instantiate (S (bs, t)) =
  VarSet.fold_left_m
    (fun typ name ->
      (* x acc -> f acc x*)
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize env ty =
  (* все переменные, которые есть в выражении, но которые не содержатся в env *)
  let free = VarSet.diff (Type.vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match TypeEnv.find xs e with
  | None -> fail (`Unbound_variable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

open Ast

let pattern_vars =
  let rec helper acc = function
    | Pat_var (Id v) -> v :: acc
    | Pat_cons (e1, e2) -> helper (helper acc e1) e2
    | Pat_tuple es -> List.fold_left helper acc es
    | _ -> acc
  in
  helper []
;;

let check_several_bounds names env ty =
  RList.fold_left
    names
    ~init:(return ([], env, ty))
    ~f:(fun (acc, env, ty) name ->
      if List.exists (String.equal name) acc
      then fail (`Several_bounds name)
      else return (name :: acc, env, ty))
;;

let const_infer fst = function
  | Const_int _ -> return (fst, tint)
  | Const_bool _ -> return (fst, tbool)
  | Const_nil ->
    let* tv = fresh_var in
    return (fst, tlist tv)
  | Const_unit -> return (fst, TPrim "unit")
;;

let pattern_infer =
  let rec helper env = function
    | Pat_const c -> const_infer env c
    | Pat_var (Id v) ->
      let* var = fresh_var in
      let env' = TypeEnv.extend env (v, S (VarSet.empty, var)) in
      return (env', var)
    | Pat_cons (p, p') as pat ->
      let* e, t = helper env p in
      let* e', t' = helper e p' in
      let* subst = unify (tlist t) t' in
      let env = TypeEnv.apply subst e' in
      let t' = Subst.apply subst t' in
      let* _, env, t' = check_several_bounds (pattern_vars pat) env t' in
      return (env, t')
    | Pat_any ->
      let* tv = fresh_var in
      return (env, tv)
    | Pat_tuple ps as pat ->
      let* env, ty =
        RList.fold_left
          ps
          ~init:(return (env, ttuple []))
          ~f:(fun (env, ty) pat ->
            let* env1, p1 = helper env pat in
            match ty with
            | TTuple l -> return (env1, ttuple (p1 :: l))
            | _ -> fail `Unreachable)
      in
      let* _, env, ty = check_several_bounds (pattern_vars pat) env ty in
      return (env, ty)
  in
  helper
;;

let infer =
  let rec helper env = function
    | Exp_constant c -> const_infer Subst.empty c
    | Exp_ident (Id v) -> lookup_env v env
    | Exp_unary_op (op, e) ->
      let exp_ty =
        match op with
        | Minus -> tint
        | Not -> tbool
      in
      let* s, t = helper env e in
      let* subst = unify t exp_ty in
      let* final_subs = Subst.compose s subst in
      return (final_subs, exp_ty)
      (* (match op with
         | Minus ->

         return (Subst.empty, tarrow tint tint)
         | Not -> return (Subst.empty, tarrow tbool tbool))*)
    | Exp_bin_op (op, e1, e2) ->
      let* arg_ty, expr_type =
        match op with
        | Asterisk | Divider | Plus | Sub -> return (tint, tint)
        | And | Or -> return (tbool, tbool)
        | Eq | Neq | Lt | Ltq | Gt | Gtq ->
          let* tv = fresh_var in
          return (tv, tbool)
      in
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper env e2 in
      let* sub1 = unify t1 arg_ty in
      let* sub2 = unify (Subst.apply sub1 t2) arg_ty in
      let* final_subs = Subst.compose_all [ s1; s2; sub1; sub2 ] in
      return (final_subs, expr_type)
      (*
         (match op with
         | Asterisk | Divider | Plus | Sub ->
         return (Subst.empty, tarrow tint (tarrow tint tint))
         | And | Or -> return (Subst.empty, tarrow tbool (tarrow tbool tbool))
         | Eq | Neq | Lt | Ltq | Gt | Gtq ->
         let* v = fresh_var in
         return (Subst.empty, tarrow v (tarrow v tbool)))*)
    | Exp_apply (e, e') ->
      let* tv = fresh_var in
      let* s1, t1 = helper env e in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e' in
      let* sub = unify (TArrow (t2, tv)) (Subst.apply s2 t1) in
      let trez = Subst.apply sub tv in
      let* final_subs = Subst.compose_all [ s1; s2; sub ] in
      return (final_subs, trez)
    | Exp_function (v, e) ->
      let* env', t = pattern_infer env v in
      let* s, t' = helper env' e in
      let ty = TArrow (Subst.apply s t, t') in
      return (s, ty)
    | Exp_ifthenelse (i, th, e) ->
      let* si, ti = helper env i in
      let* st, tt = helper env th in
      let* se, te = helper env e in
      let* sub = unify ti tbool in
      let* tv = fresh_var in
      let* sub1 = unify tv tt in
      let* sub2 = unify tv te in
      let* final_subs = Subst.compose_all [ si; st; se; sub; sub1; sub2 ] in
      return (final_subs, Subst.apply final_subs tt)
    | Exp_let ({ d_rec = Nonrecursive; d_pat; d_expr }, e) ->
      let* s1, t1 = helper env d_expr in
      let scheme = generalize (TypeEnv.apply s1 env) t1 in
      let* env1, t2 = pattern_infer env d_pat in
      let env2 = TypeEnv.extend1 scheme env1 d_pat in
      let* sub = unify t2 t1 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply sub1 env2 in
      let* s2, t2 = helper env3 e in
      let* final_subs = Subst.compose_all [ sub1; s2 ] in
      return (final_subs, t2)
    | Exp_let ({ d_rec = Recursive; d_pat; d_expr }, e) ->
      (match d_pat with
       | Pat_var (Id v) ->
         let* tv = fresh_var in
         let env = TypeEnv.extend env (v, S (VarSet.empty, tv)) in
         let* s1, t1 = helper env d_expr in
         let* s2 = unify (Subst.apply s1 tv) t1 in
         let* s = Subst.compose s2 s1 in
         let env = TypeEnv.apply s env in
         let t2 = generalize env (Subst.apply s tv) in
         let* s2, t2 = helper TypeEnv.(extend (apply s env) (v, t2)) e in
         let* final_subs = Subst.compose s s2 in
         return (final_subs, t2)
       | _ -> fail `No_variable_rec)
    (*    | Exp_list es ->
          (match es with
          | [] ->
          let* tv = fresh_var in
          return (Subst.empty, tv)
          | h :: tl ->
          let* s, t = helper env h in
          let* sub =
          RList.fold_left tl ~init:(return s) ~f:(fun acc x ->
          let* s', t' = helper env x in
          let* sub = unify t t' in
          let* final_subs = Subst.compose_all [ sub; s'; acc ] in
          return final_subs)
          in
          let trez = tlist t in
          return (sub, trez)) *)
    | Exp_match (match_exp, cases) ->
      let* s, tcond = helper env match_exp in
      let env = TypeEnv.apply s env in
      let* tv = fresh_var in
      (match cases with
       | [] -> fail `Unreachable
       | cs ->
         RList.fold_left
           cs
           ~init:(return (s, tv))
           ~f:(fun (s, t) (p, e) ->
             let* env, tp = pattern_infer env p in
             let* s' = unify tcond tp in
             let* sub, te = helper env e in
             let* sub' = unify t te in
             let* final_subs = Subst.compose_all [ sub'; sub; s'; s ] in
             return (final_subs, Subst.apply final_subs t)))
    | Exp_tuple es ->
      let* s, t =
        RList.fold_left
          ~f:(fun (s, ty) e ->
            let* s', ty' = helper env e in
            let* sub = Subst.compose s' s in
            return (sub, ty' :: ty))
          ~init:(return (Subst.empty, []))
          es
      in
      return (s, ttuple (List.rev_map (Subst.apply s) t))
    | Exp_list (e, e') ->
      let* s1, t1 = helper env e in
      let* s2, t2 = helper env e' in
      let* subst = unify (tlist t1) t2 in
      let trez = Subst.apply subst t2 in
      let* final_subs = Subst.compose_all [ s1; s2; subst ] in
      return (final_subs, trez)
    | Exp_send (_, _) -> fail `Not_implemented
    | Exp_object _ -> fail `Not_implemented
  in
  helper
;;

let w e = Result.map snd (run (infer TypeEnv.empty e))

let expr_type env e =
  let* _, _ = infer env e in
  return env
;;

(* TODO: remove copy-paste *)
let value_type env = function
  | { d_rec = Nonrecursive; d_pat; d_expr } ->
    let* s1, t1 = infer env d_expr in
    let scheme = generalize (TypeEnv.apply s1 env) t1 in
    let* env1, t2 = pattern_infer env d_pat in
    let env2 = TypeEnv.extend1 scheme env1 d_pat in
    let* sub = unify t2 t1 in
    let* sub1 = Subst.compose sub s1 in
    let env3 = TypeEnv.apply sub1 env2 in
    return env3
  | { d_rec = Recursive; d_pat; d_expr } ->
    (match d_pat with
     | Pat_var (Id v) ->
       let* tv = fresh_var in
       let env = TypeEnv.extend env (v, S (VarSet.empty, tv)) in
       let* s1, t1 = infer env d_expr in
       let* s2 = unify (Subst.apply s1 tv) t1 in
       let* s = Subst.compose s2 s1 in
       let env = TypeEnv.apply s env in
       return env
     | _ -> fail `No_variable_rec)
;;

let structure_item_type env = function
  | Str_eval e -> expr_type env e
  | Str_value d -> value_type env d
;;

let check_program program =
  let env = TypeEnv.empty in
  let helper env =
    RList.fold_left program ~init:(return env) ~f:(fun env item ->
      structure_item_type env item)
  in
  run (helper env)
;;
