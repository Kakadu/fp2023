(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Typedtree
open Errors

module R = struct
  type ('a, 'e) t = int -> int * ('a, 'e) Result.t

  let return : 'a -> ('a, 'e) t = fun x st -> st, Result.Ok x
  let fail : 'e -> ('a, 'e) t = fun e st -> st, Result.Error e

  let ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t =
    fun m f st ->
    let st, r = m st in
    match r with
    | Ok a -> f a st
    | Error e -> fail e st
  ;;

  let ( let* ) = ( >>= )
end

let fresh st = st + 1, Ok st
let run m = snd (m 0)

module VarSet = struct
  include Set

  type t = (int, Int.comparator_witness) Set.t

  let empty = Set.empty (module Int)
end

module Type = struct
  let rec occurs_in v = function
    | TBool | TInt | TEmpty -> false
    | TVar b -> b = v
    | TList x -> occurs_in v x
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
  ;;

  let free_vars =
    let rec helper acc = function
      | TBool | TInt | TEmpty -> acc
      | TVar b -> VarSet.add acc b
      | TList x -> helper acc x
      | TArrow (l, r) ->
        let lvars = helper acc l in
        helper lvars r
    in
    helper VarSet.empty
  ;;
end

module Subst = struct
  open R

  type t = (int, typ, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)
  let singleton (k, v) = return (Map.singleton (module Int) k v)

  let apply (subst : t) =
    let rec helper = function
      | (TInt | TBool | TEmpty) as other -> other
      | TVar b ->
        (match Map.find subst b with
         | Some v -> v
         | None -> TVar b)
      | TList x -> TList (helper x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TInt, TInt | TBool, TBool | TEmpty, TEmpty -> return empty
    | TVar l, TVar r when l = r -> return empty
    | TVar b, t | t, TVar b -> singleton (b, t)
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TList x1, TList x2 -> unify x1 x2
    | _ -> fail (Ivalid_format_concat (l, r))

  and extend (subst : t) (k, v) : (t, error) R.t =
    match Map.find subst k with
    | Some v2 ->
      let* subst2 = unify v v2 in
      compose subst subst2
    | None ->
      let v = apply subst v in
      let* subst2 = singleton (k, v) in
      Map.fold subst ~init:(return subst2) ~f:(fun ~key:k ~data:v acc ->
        let* acc = acc in
        let v = apply subst2 v in
        if Type.occurs_in k v
        then fail (Occurs_check (k, v))
        else return (Map.set acc ~key:k ~data:v))

  and compose subst1 subst2 =
    Map.fold subst1 ~init:(return subst2) ~f:(fun ~key:k ~data:v acc ->
      let* acc = acc in
      extend acc (k, v))
  ;;

  let compose_all substs =
    List.fold_left substs ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module Scheme = struct
  type t = S of VarSet.t * typ

  let free_vars (S (s, typ)) = VarSet.diff (Type.free_vars typ) s

  let apply (S (s, typ)) subst =
    let subst2 = VarSet.fold s ~init:subst ~f:(fun acc k -> Map.remove acc k) in
    S (s, Subst.apply subst2 typ)
  ;;
end

module TypeEnv = struct
  type t = (id, Scheme.t, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let free_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:sch acc ->
      VarSet.union acc (Scheme.free_vars sch))
  ;;

  let extend env (id, sch) = Map.set env ~key:id ~data:sch
end

open R

let varf = fresh >>= fun x -> return (TVar x)

let generalize env typ =
  let free = VarSet.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  Scheme.S (free, typ)
;;

let infer_pattern =
  let helper env = function
    | PAny ->
      let* tv = varf in
      return (env, tv)
    | PConst (Int _) -> return (env, TInt)
    | PConst (Bool _) -> return (env, TBool)
    | PConst Empty -> return (env, TEmpty)
    | PVar x ->
      (match Map.find env x with
       | None ->
         let* tv = varf in
         let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
         return (env, tv)
       | Some (Scheme.S (_, typ)) -> return (env, typ))
  in
  helper
;;

let lookup_env env id =
  match Map.find env id with
  | Some sch ->
    let* typ =
      let (Scheme.S (s, typ)) = sch in
      VarSet.fold s ~init:(return typ) ~f:(fun typ name ->
        let* typ = typ in
        let* fv = varf in
        let* subst = Subst.singleton (name, fv) in
        return (Subst.apply subst typ))
    in
    return (Subst.empty, typ)
  | None -> fail (Unbound_value id)
;;

let inferencer =
  let rec helper env = function
    | Const (Int _) -> return (Subst.empty, TInt)
    | Const (Bool _) -> return (Subst.empty, TBool)
    | Const Empty -> return (Subst.empty, TEmpty)
    | Var x -> lookup_env env x
    | Fun (x, e) ->
      let* tv = varf in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s, typ = helper env2 e in
      let res_typ = TArrow (Subst.apply s tv, typ) in
      return (s, res_typ)
    | Binop (op, l, r) -> infer_binop env op l r
    | List x ->
      (match x with
       | [] ->
         let* tv = varf in
         return (Subst.empty, TList tv)
       | h :: tl ->
         let* final_subst, res_typ =
           List.fold_left tl ~init:(helper env h) ~f:(fun acc e ->
             let* subst, typ = acc in
             let* subst1, typ1 = helper env e in
             let* subst2 = Subst.unify typ typ1 in
             let* final_subst = Subst.compose_all [ subst; subst1; subst2 ] in
             let res_typ = Subst.apply final_subst typ in
             return (final_subst, res_typ))
         in
         return (final_subst, TList res_typ))
    | IfThenElse (bin_op, e1, e2) ->
      let* subst1, typ1 = helper env bin_op in
      let* subst2, typ2 = helper env e1 in
      let* subst3, typ3 = helper env e2 in
      let* subst4 = Subst.unify typ1 TBool in
      let* subst5 = Subst.unify typ2 typ3 in
      let* final_subst = Subst.compose_all [ subst1; subst2; subst3; subst4; subst5 ] in
      return (final_subst, Subst.apply subst5 typ3)
    | Let (NoRec, _, e1, None) -> helper env e1
    | Let (NoRec, x, e1, Some e2) ->
      let* subst1, typ1 = helper env e1 in
      let env2 = Map.map env ~f:(fun sch -> Scheme.apply sch subst1) in
      let typ2 = generalize env2 typ1 in
      let env3 = TypeEnv.extend env2 (x, typ2) in
      let* subst2, typ3 = helper env3 e2 in
      let* final_subst = Subst.compose subst1 subst2 in
      return (final_subst, typ3)
    | Let (Rec, x, e1, None) ->
      let* tv = varf in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* subst1, typ1 = helper env e1 in
      let* subst2 = Subst.unify (Subst.apply subst1 tv) typ1 in
      let* final_subst = Subst.compose subst1 subst2 in
      return (final_subst, Subst.apply final_subst tv)
    | Let (Rec, x, e1, Some e2) ->
      let* tv = varf in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* subst1, typ1 = helper env e1 in
      let* subst2 = Subst.unify (Subst.apply subst1 tv) typ1 in
      let* subst = Subst.compose subst1 subst2 in
      let env = Map.map env ~f:(fun sch -> Scheme.apply sch subst) in
      let typ2 = generalize env (Subst.apply subst tv) in
      let env = TypeEnv.extend env (x, typ2) in
      let* subst2, typ2 = helper env e2 in
      let* final_subst = Subst.compose subst subst2 in
      return (final_subst, typ2)
    | App (e1, e2) ->
      let* subst1, typ1 = helper env e1 in
      let env' = Map.map env ~f:(fun sch -> Scheme.apply sch subst1) in
      let* subst2, typ2 = helper env' e2 in
      let* tv = varf in
      let* subst3 = Subst.unify (Subst.apply subst2 typ1) (TArrow (typ2, tv)) in
      let res_typ = Subst.apply subst3 tv in
      let* final_subst = Subst.compose_all [ subst1; subst2; subst3 ] in
      return (final_subst, res_typ)
    | Match (c, list) ->
      let* c_subst, c_typ = helper env c in
      let* tv = varf in
      let* e_subst, e_typ =
        List.fold_left
          list
          ~init:(return (c_subst, tv))
          ~f:(fun acc (pat, e) ->
            let* subst, typ = acc in
            let* pat_env, pat_typ = infer_pattern env pat in
            let* subst2 = Subst.unify c_typ pat_typ in
            let* subst3, e_typ = helper pat_env e in
            let* subst4 = Subst.unify typ e_typ in
            let* final_subst = Subst.compose_all [ subst; subst2; subst3; subst4 ] in
            return (final_subst, Subst.apply final_subst typ))
      in
      let* final_subst = Subst.compose c_subst e_subst in
      return (final_subst, Subst.apply final_subst e_typ)
  and infer_binop env op l r =
    let* l_subst, l_typ = helper env l in
    let* r_subst, r_typ = helper env r in
    match op with
    | Les | Leq | Gre | Geq | Eq | Neq ->
      let* subst = Subst.unify l_typ r_typ in
      let* final_subst = Subst.compose_all [ l_subst; r_subst; subst ] in
      return (final_subst, TBool)
    | _ ->
      let* subst1 = Subst.unify l_typ TInt in
      let* subst2 = Subst.unify r_typ TInt in
      let* final_subst = Subst.compose_all [ l_subst; r_subst; subst1; subst2 ] in
      return (final_subst, TInt)
  in
  helper
;;

let infer env program =
  let check_expr env e =
    let* _, typ = inferencer env e in
    match e with
    | Let (_, x, _, None) ->
      let env = TypeEnv.extend env (x, Scheme.S (VarSet.empty, typ)) in
      return (env, typ)
    | _ -> return (env, typ)
  in
  let* env, tys =
    List.fold_left
      program
      ~init:(return (env, []))
      ~f:(fun acc e ->
        let* env, program = acc in
        let* env, typ = check_expr env e in
        return (env, (e, typ) :: program))
  in
  return (env, List.rev tys)
;;

let infer_program ?(env = TypeEnv.empty) program = run (infer env program)
