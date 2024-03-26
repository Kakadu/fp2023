(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Type inference (unification).
    For better understanding see:
    * Kakadu implementation of miniml language inferencer
    [here](https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml) *)

open Base
open Typeandprinter
open Errorinter

module R : sig
  type 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : Errorinter.error_infer -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, Errorinter.error_infer) result
end = struct
  type 'a t = int -> int * ('a, error_infer) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = typ

  let rec occurs_in v =
    let occurs_in_list ts =
      List.fold ts ~init:false ~f:(fun acc t -> acc || occurs_in v t)
    in
    function
    | Ty_var b -> b = v
    | Arrow (l, r) -> occurs_in v l || occurs_in v r
    | List_typ t -> occurs_in v t
    | Tuple_typ ts -> occurs_in_list ts
    | Prim _ -> false
  ;;
end

let fold_left map ~init ~f =
  Map.Poly.fold map ~init ~f:(fun ~key ~data acc ->
    let open R.Syntax in
    let* acc = acc in
    f key data acc)
;;

module Subst = struct
  open R
  open R.Syntax

  type t = (fresh, typ) Map.Poly.t

  let pp ppf subst =
    let open Format in
    Map.Poly.iteri subst ~f:(fun ~key ~data ->
      fprintf ppf "%d -> %a@\n" key Typeandprinter.pp_typ_binder data)
  ;;

  let empty = Map.Poly.empty
  let mapping k v = if Type.occurs_in k v then fail OccursCheck else return (k, v)

  let singleton k v =
    let* f, t = mapping k v in
    return @@ Map.Poly.singleton f t
  ;;

  let remove m f = Map.Poly.remove m f

  let apply subst =
    let rec helper =
      let apply_to_list l = List.map ~f:helper l in
      function
      | Ty_var b as ty ->
        (match Map.Poly.find_exn subst b with
         | exception Not_found_s _ -> ty
         | x -> x)
      | Arrow (l, r) -> Arrow (helper l, helper r)
      | List_typ t -> List_typ (helper t)
      | Tuple_typ l -> Tuple_typ (apply_to_list l)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    let unify_lists l1 l2 =
      let subs =
        List.fold2 l1 l2 ~init:(return empty) ~f:(fun subs a b ->
          let* subs = subs in
          let sa = apply subs a in
          let sb = apply subs b in
          let* sub1 = unify sa sb in
          compose subs sub1)
      in
      match subs with
      | Ok res -> res
      | Unequal_lengths -> fail (UnificationFailed (l, r))
    in
    match l, r with
    | Prim l, Prim r when String.equal l r -> return empty
    | Prim _, Prim _ -> fail (UnificationFailed (l, r))
    | Ty_var a, Ty_var b when Int.equal a b -> return empty
    | Ty_var b, t | t, Ty_var b -> singleton b t
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | List_typ a, List_typ b -> unify a b
    | Tuple_typ a, Tuple_typ b -> unify_lists a b
    | _ -> fail (UnificationFailed (l, r))

  and extend key value extensible_subst =
    match Map.Poly.find extensible_subst key with
    | None ->
      let v = apply extensible_subst value in
      let* s2 = singleton key v in
      fold_left extensible_subst ~init:(return s2) ~f:(fun key value acc ->
        let v = apply s2 value in
        let* mapk, mapv = mapping key v in
        match Map.Poly.add acc ~key:mapk ~data:mapv with
        | `Ok map -> return map
        | `Duplicate -> return acc)
    | Some v2 ->
      let* s2 = unify value v2 in
      compose extensible_subst s2

  and compose s1 s2 = fold_left s2 ~init:(return s1) ~f:extend

  let compose_all s1 =
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    in
    fold_left s1 ~init:(return empty) ~f:compose
  ;;
end

module VarSet = struct
  include VarSetInit

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

type binder_set = VarSet.t
type scheme = S of binder_set * typ

module Scheme = struct
  type t = scheme

  let apply sub (S (names, typ)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 typ)
  ;;
end

type environment = (string * scheme) list

module TypeEnv = struct
  type t = environment

  let extend e h = h :: e
  let empty = []
  let apply subst env = List.Assoc.map env ~f:(Scheme.apply subst)
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> Ty_var n

let instantiate : scheme -> typ R.t =
  fun (S (names, ty)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    names
    (return ty)
;;

let lookup_env var env =
  match List.Assoc.find_exn env ~equal:String.equal var with
  | (exception Caml.Not_found) | (exception Not_found_s _) -> fail (NoVariable var)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let infer =
  let open Ast in
  let rec (pattern_helper : TypeEnv.t -> Ast.pattern -> (TypeEnv.t * typ) R.t) =
    fun env ->
    let rec eval_list_helper envpat =
      let* env, patterns = envpat in
      match patterns with
      | [] -> return (env, [])
      | hd :: tl ->
        let* envhd, tyhd = pattern_helper env hd in
        let new_envpat = return (envhd, tl) in
        let* envtl, tytl = eval_list_helper new_envpat in
        return (envtl, tyhd :: tytl)
    in
    function
    | Const const ->
      (match const with
       | CBool _ -> return (env, Prim "bool")
       | CInt _ -> return (env, Prim "int")
       | CString _ -> return (env, Prim "string"))
    | Var id ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (id, S (VarSet.empty, tv)) in
      return (env, tv)
    | Tuple tuple ->
      let* finenv, fintys = eval_list_helper @@ return (env, tuple) in
      return (finenv, Tuple_typ fintys)
    | Wild ->
      let* ty = fresh_var in
      return (env, ty)
    | List a ->
      let* env, ty1 = pattern_helper env (List.hd_exn a) in
      let ty1 = List_typ ty1 in
      let* env, ty2 = pattern_helper env (List.hd_exn (List.tl_exn a)) in
      let* subst = unify ty1 ty2 in
      let finenv = TypeEnv.apply subst env in
      return (finenv, Subst.apply subst ty1)
    | Case _ -> fail NotImplemented
  in
  let rec (helper : TypeEnv.t -> Ast.expr -> (Subst.t * typ) R.t) =
    fun env -> function
    | VarExpr x -> lookup_env x env
    | ConstExpr const ->
      (match const with
       | CBool _ -> return (Subst.empty, Prim "bool")
       | CInt _ -> return (Subst.empty, Prim "int")
       | CString _ -> return (Subst.empty, Prim "string"))
    | BinExpr (op, left, right) ->
      let* subst_left, typ_left = helper env left in
      let* subst_right, typ_right = helper env right in
      (match op with
       | Add | Sub | Mul | Div | Mod ->
         let* subst' = unify typ_left (Prim "int") in
         let* subst'' = unify typ_right (Prim "int") in
         let* final_subst =
           Subst.compose_all [ subst'; subst''; subst_left; subst_right ]
         in
         return (final_subst, Prim "int")
       | Eq | Less | LEq | Gre | GEq | NEq | And | Or ->
         let* subst' = unify typ_left typ_right in
         let* final_subst = Subst.compose_all [ subst'; subst_left; subst_right ] in
         return (final_subst, Prim "bool"))
    | AppExpr (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (Arrow (t2, tv)) in
      let typedres = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, typedres)
    | IfExpr (c, th, el) ->
      let* s1, t1 = helper env c in
      let* s2, t2 = helper env th in
      let* s3, t3 = helper env el in
      let* s4 = unify t1 (Prim "bool") in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return (final_subst, Subst.apply final_subst t2)
    | LetExpr (is_rec, name, expr) ->
      if not is_rec
      then helper env expr
      else
        let* tv = fresh_var in
        let env = TypeEnv.extend env (name, S (VarSet.empty, tv)) in
        helper env expr
    | FunExpr (arg, e) ->
      let* env, t1 = pattern_helper env arg in
      let* s, t2 = helper env e in
      let typedres = Arrow (Subst.apply s t1, t2) in
      return (s, typedres)
    | MatchExpr (cond, matches) ->
      let* cond_sub, cond_ty = helper env cond in
      let env = TypeEnv.apply cond_sub env in
      let rec matches_helper = function
        | [] -> fail EmptyPattern
        | (pat, expr) :: [] ->
          let* pat_env, pat_ty = pattern_helper env pat in
          let* s1 = unify cond_ty pat_ty in
          let* s2, expr_ty = helper (TypeEnv.apply s1 pat_env) expr in
          let* finalsub = Subst.compose s1 s2 in
          return (finalsub, Subst.apply finalsub expr_ty, pat_ty)
        | hd :: tl ->
          let* s1, ty1, pattern1 = matches_helper [ hd ] in
          let* s2, ty2, _ = matches_helper tl in
          let* s3 = return Subst.empty in
          let* s4 = unify ty1 ty2 in
          let* finalsubst = Subst.compose_all [ s1; s2; s3; s4 ] in
          return (finalsubst, Subst.apply s3 ty1, pattern1)
      in
      let* match_sub, match_ty, _ = matches_helper matches in
      let* finalmatchsub = Subst.compose cond_sub match_sub in
      return (finalmatchsub, Subst.apply finalmatchsub match_ty)
    | ListExpr a ->
      if (List.length a == 0) then return (Subst.empty, Prim "'a list") else 
      let* s1, t1 = helper env (List.hd_exn a) in
      let t1 = List_typ t1 in
      let* subst = Subst.compose_all [ s1 ] in
      return (subst, Subst.apply subst t1)
    | CaseExpr _ -> fail NotImplemented
    | TupleExpr tuple ->
      let* s, t =
        List.fold
          tuple
          ~init:(return (Subst.empty, []))
          ~f:(fun acc expr ->
            let* tuple_s, tuple = acc in
            let* s, t = helper env expr in
            let* subst = Subst.compose s tuple_s in
            return (subst, t :: tuple))
      in
      return (s, Tuple_typ (List.rev t))
    | LetActExpr (_, _) -> fail NotImplemented
  in
  helper
;;

let empty : environment = TypeEnv.empty

let check_type env expr =
  let* _, typ = infer env expr in
  match expr with
  | LetExpr (_, name, _) ->
    let env = TypeEnv.extend env (name, S (VarSet.empty, typ)) in
    return (env, typ)
  | _ -> return (env, typ)
;;

let check_types env program =
  let rec helper env = function
    | [] -> fail EmptyProgram
    | hd :: [] -> check_type env hd
    | hd :: tl ->
      let* env, _ = check_type env hd in
      helper env tl
  in
  helper env program
;;

let check_types ?(env : environment = empty) e =
  Result.map (run (check_types env e)) ~f:Fun.id
;;