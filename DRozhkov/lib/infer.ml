(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Typedtree

module VarSet = struct
  include Set

  type t = (int, Int.comparator_witness) Set.t

  let empty = Set.empty (module Int)
end

module R = struct
  type ('a, 'expr) t = int -> int * ('a, 'expr) Result.t

  let return x st = st, Result.Ok x
  let fail expr st = st, Result.Error expr

  let ( >>= ) m f st =
    let st, r = m st in
    match r with
    | Ok a -> f a st
    | Error expr -> fail expr st
  ;;

  let ( let* ) = ( >>= )
  let fresh st = st + 1, Ok st
  let run m = snd (m 0)
end

module Type = struct
  let rec occurs_in v = function
    | TBool | TInt | TUnit -> false
    | TVar b -> b = v
    | TList x -> occurs_in v x
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
  ;;

  let free_vars =
    let rec helper acc = function
      | TBool | TInt | TUnit -> acc
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

  let singleton (k, v) =
    if Type.occurs_in k v
    then fail Occurs_check
    else return (Map.singleton (module Int) k v)
  ;;

  let apply (sub : t) =
    let rec helper = function
      | (TInt | TBool | TUnit) as other -> other
      | TVar b ->
        (match Map.find sub b with
         | Some v -> v
         | None -> TVar b)
      | TList x -> TList (helper x)
      | TArrow (l, r) -> TArrow (helper l, helper r)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TInt, TInt | TBool, TBool | TUnit, TUnit -> return empty
    | TVar l, TVar r when l = r -> return empty
    | TVar b, t | t, TVar b -> singleton (b, t)
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* sub1 = unify l1 l2 in
      let* sub2 = unify (apply sub1 r1) (apply sub1 r2) in
      compose sub1 sub2
    | TList x1, TList x2 -> unify x1 x2
    | _ -> fail (Unification_failed (l, r))

  and extend (sub : t) (k, v) : (t, error) R.t =
    match Map.find sub k with
    | Some v2 ->
      let* sub2 = unify v v2 in
      compose sub sub2
    | None ->
      let v = apply sub v in
      let* sub2 = singleton (k, v) in
      Map.fold sub ~init:(return sub2) ~f:(fun ~key:k ~data:v acc ->
        let* acc = acc in
        let v = apply sub2 v in
        if Type.occurs_in k v
        then fail Occurs_check
        else return (Map.set acc ~key:k ~data:v))

  and compose sub1 sub2 =
    Map.fold sub1 ~init:(return sub2) ~f:(fun ~key:k ~data:v acc ->
      let* acc = acc in
      extend acc (k, v))
  ;;

  let compose_all substs =
    List.fold_left substs ~init:(return empty) ~f:(fun acc sub ->
      let* acc = acc in
      compose acc sub)
  ;;
end

open R

type scheme = S of VarSet.t * typ

let fresh_v = fresh >>= fun x -> return (TVar x)

module Scheme = struct
  type t = scheme

  let free_vars (S (s, typ)) = VarSet.diff (Type.free_vars typ) s

  let apply (S (s, typ)) sub =
    let sub2 = VarSet.fold s ~init:sub ~f:(fun acc k -> Map.remove acc k) in
    S (s, Subst.apply sub2 typ)
  ;;
end

module TypeEnv = struct
  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let free_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:sch acc ->
      VarSet.union acc (Scheme.free_vars sch))
  ;;

  let apply env sub = Base.Map.map env ~f:(fun sch -> Scheme.apply sch sub)
  let extend env (id, sch) = Map.set env ~key:id ~data:sch
end

let generalize env typ =
  let free = VarSet.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  S (free, typ)
;;

let infer_pattern =
  let rec helper env = function
    | PDash ->
      let* tv = fresh_v in
      return (env, tv)
    | PConst (Int _) -> return (env, TInt)
    | PConst (Bool _) -> return (env, TBool)
    | PConst Unit -> return (env, TUnit)
    | PConst Nil ->
      let* typ = fresh_v in
      return (env, TList typ)
    | PVar x ->
      (match Map.find env x with
       | None ->
         let* tv = fresh_v in
         let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
         return (env, tv)
       | Some (S (_, typ)) -> return (env, typ))
    | PList (t1, t2) ->
      let* env, tt1 = helper env t1 in
      let* env', tt2 = helper env t2 in
      let* subst = Subst.unify tt1 tt2 in
      let env'' = TypeEnv.apply env' subst in
      return (env'', TList (Subst.apply subst tt1))
  in
  helper
;;

let inferencer =
  let rec helper env = function
    | EConst (Int _) -> return (Subst.empty, TInt)
    | EConst Nil ->
      let* tv = fresh_v in
      return (Subst.empty, TList tv)
    | EConst (Bool _) -> return (Subst.empty, TBool)
    | EConst Unit -> return (Subst.empty, TBool)
    | Var x ->
      (match Map.find env x with
       | Some schem ->
         let* typ =
           let (S (s, typ)) = schem in
           VarSet.fold s ~init:(return typ) ~f:(fun typ name ->
             let* typ = typ in
             let* fv = fresh_v in
             let* sub = Subst.singleton (name, fv) in
             return (Subst.apply sub typ))
         in
         return (Subst.empty, typ)
       | None -> fail (No_variable x))
    | EFun (x, expr) ->
      let* tv = fresh_v in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s, typ = helper env2 expr in
      let res_typ = TArrow (Subst.apply s tv, typ) in
      return (s, res_typ)
    | EBinop (l, op, r) ->
      let* l_subst, l_typ = helper env l in
      let* r_subst, r_typ = helper env r in
      (match op with
       | Less | LeEq | More | MoEq | Equally | NEqually | Or | And ->
         let* sub = Subst.unify l_typ r_typ in
         let* final_sub = Subst.compose_all [ l_subst; r_subst; sub ] in
         return (final_sub, TBool)
       | _ ->
         let* sub1 = Subst.unify l_typ TInt in
         let* sub2 = Subst.unify r_typ TInt in
         let* final_sub = Subst.compose_all [ l_subst; r_subst; sub1; sub2 ] in
         return (final_sub, TInt))
    | EList x ->
      (match x with
       | [] ->
         let* tv = fresh_v in
         return (Subst.empty, TList tv)
       | h :: tl ->
         let* final_sub, res_typ =
           List.fold_left tl ~init:(helper env h) ~f:(fun acc expr ->
             let* sub, typ = acc in
             let* sub1, typ1 = helper env expr in
             let* sub2 = Subst.unify typ typ1 in
             let* final_sub = Subst.compose_all [ sub; sub1; sub2 ] in
             let res_typ = Subst.apply final_sub typ in
             return (final_sub, res_typ))
         in
         return (final_sub, TList res_typ))
    | EIfThenElse (bin_op, e1, e2) ->
      let* sub1, typ1 = helper env bin_op in
      let* sub2, typ2 = helper env e1 in
      let* sub3, typ3 = helper env e2 in
      let* sub4 = Subst.unify typ1 TBool in
      let* sub5 = Subst.unify typ2 typ3 in
      let* final_sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
      return (final_sub, Subst.apply sub5 typ3)
    | EApp (e1, e2) ->
      let* sub1, typ1 = helper env e1 in
      let env' = Map.map env ~f:(fun sch -> Scheme.apply sch sub1) in
      let* sub2, typ2 = helper env' e2 in
      let* tv = fresh_v in
      let* sub3 = Subst.unify (Subst.apply sub2 typ1) (TArrow (typ2, tv)) in
      let res_typ = Subst.apply sub3 tv in
      let* final_sub = Subst.compose_all [ sub1; sub2; sub3 ] in
      return (final_sub, res_typ)
    | ELet (NoRec, _, e1, None) -> helper env e1
    | ELet (NoRec, x, e1, Some e2) ->
      let* sub1, typ1 = helper env e1 in
      let env2 = Map.map env ~f:(fun sch -> Scheme.apply sch sub1) in
      let typ2 = generalize env2 typ1 in
      let env3 = TypeEnv.extend env2 (x, typ2) in
      let* sub2, typ3 = helper env3 e2 in
      let* final_sub = Subst.compose sub1 sub2 in
      return (final_sub, typ3)
    | ELet (Rec, x, e1, None) ->
      let* final_sub, tv = infer_recursively env x e1 in
      return (final_sub, Subst.apply final_sub tv)
    | ELet (Rec, x, e1, Some e2) ->
      let* tv = fresh_v in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* sub1, ty1 = helper env e1 in
      let* sub2 = Subst.unify (Subst.apply sub1 tv) ty1 in
      let* sub = Subst.compose sub1 sub2 in
      let env = TypeEnv.apply env sub in
      let ty2 = generalize env (Subst.apply sub tv) in
      let* sub2, ty2 = helper TypeEnv.(extend (apply env sub) (x, ty2)) e2 in
      let* final_sub = Subst.compose sub sub2 in
      return (final_sub, ty2)
    | EMatch (c, list) ->
      let* c_sub, c_typ = helper env c in
      let* tv = fresh_v in
      let* e_sub, e_typ =
        List.fold_left
          list
          ~init:(return (c_sub, tv))
          ~f:(fun acc (pat, expr) ->
            let* sub, typ = acc in
            let* pat_env, pat_typ = infer_pattern env pat in
            let* sub2 = Subst.unify c_typ pat_typ in
            let* sub3, e_typ = helper pat_env expr in
            let* sub4 = Subst.unify typ e_typ in
            let* final_sub = Subst.compose_all [ sub; sub2; sub3; sub4 ] in
            return (final_sub, Subst.apply final_sub typ))
      in
      let* final_sub = Subst.compose c_sub e_sub in
      return (final_sub, Subst.apply final_sub e_typ)
  and infer_recursively env x expr =
    let* tv = fresh_v in
    let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
    let* sub1, typ1 = helper env expr in
    let* sub2 = Subst.unify (Subst.apply sub1 tv) typ1 in
    let* final_sub = Subst.compose sub1 sub2 in
    return (final_sub, tv)
  in
  helper
;;

let infer env program =
  let check_expr env expr =
    let* _, typ = inferencer env expr in
    match expr with
    | ELet (_, x, _, None) ->
      let env = TypeEnv.extend env (x, S (VarSet.empty, typ)) in
      return (env, typ)
    | _ -> return (env, typ)
  in
  let* env, tys =
    List.fold_left
      program
      ~init:(return (env, []))
      ~f:(fun acc expr ->
        let* env, program = acc in
        let* env, typ = check_expr env expr in
        return (env, (expr, typ) :: program))
  in
  return (env, List.rev tys)
;;

let run_infer ?(env = TypeEnv.empty) program = run (infer env program)
