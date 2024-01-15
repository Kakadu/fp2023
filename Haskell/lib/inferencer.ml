(** Copyright 2023-2024, Danil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* The basis for algorithm is taken from
   https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml *)

(* Some bugs are left unintentionally. *)

open Typedtree
open Ast

type error =
  | OccursCheck
  | NoVariable of string
  | UnificationFailed of ty * ty

let pp_error fmt = function
  | OccursCheck -> Format.printf "Occurs check"
  | NoVariable s -> Format.fprintf fmt "Variable not in scope: %s" s
  | UnificationFailed (l, r) ->
    Format.fprintf
      fmt
      "This expression has type (%a) but an expression was expected of type (%a)"
      pp_type
      l
      pp_type
      r
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
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
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TyVar b -> b = v
    | TyArrow (l, r) -> occurs_in v l || occurs_in v r
    | TyLit _ -> false
    | TyList ty -> occurs_in v ty
    | TyTuple types -> List.exists (fun ty -> occurs_in v ty) types
  ;;

  let free_vars =
    let rec helper acc = function
      | TyVar b -> VarSet.add b acc
      | TyArrow (l, r) -> helper (helper acc l) r
      | TyLit _ -> acc
      | TyList ty -> helper acc ty
      | TyTuple types -> List.fold_left (fun acc ty -> helper acc ty) acc types
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : t -> fresh -> ty

  val find : t -> fresh -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax
  open Base

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Base.Int)

  let singleton k v =
    if Type.occurs_in k v
    then fail OccursCheck
    else return @@ Map.singleton (module Base.Int) k v
  ;;

  let find_exn = Map.find_exn
  let find = Map.find
  let remove = Map.remove

  let apply s =
    let rec helper = function
      | TyVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | TyArrow (l, r) -> TyArrow (helper l, helper r)
      | TyList ty -> TyList (helper ty)
      | TyTuple tys -> TyTuple (List.map tys ~f:helper)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TyLit l, TyLit r when String.equal l r -> return empty
    | TyVar a, TyVar b when Int.equal a b -> return empty
    | TyVar b, t | t, TyVar b -> singleton b t
    | TyArrow (hd1, tl1), TyArrow (hd2, tl2) ->
      let* subs1 = unify hd1 hd2 in
      let* subs2 = unify (apply subs1 tl1) (apply subs1 tl2) in
      compose subs1 subs2
    | TyList t1, TyList t2 -> unify t1 t2
    | TyTuple ts1, TyTuple ts2 when List.length ts1 = List.length ts2 ->
      let rec unify_tuples ts1 ts2 acc =
        match ts1, ts2 with
        | [], [] -> return acc
        | t1 :: ts1', t2 :: ts2' ->
          let* subs = unify t1 t2 in
          let* acc' = compose subs acc in
          unify_tuples
            (List.map ~f:(apply subs) ts1')
            (List.map ~f:(apply subs) ts2')
            acc'
        | _, _ -> fail @@ UnificationFailed (l, r)
        (* todo: mismatch *)
      in
      unify_tuples ts1 ts2 empty
    | _ -> fail @@ UnificationFailed (l, r)

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

module VarSet = struct
  include VarSet

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

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;
end

module TypeEnv = struct
  open Base
  open Ast

  type t = (id, scheme, String.comparator_witness) Map.t

  let extend env (id, scheme) = Map.set env ~key:id ~data:scheme
  let empty = Map.empty (module String)

  let rec extend_by_pattern (S (bs, ty) as scheme) acc pat =
    match pat, ty with
    | PatVar v, _ -> extend acc (v, scheme)
    | PatCons (h, tl), TyList t ->
      let env = extend_by_pattern (S (bs, t)) acc h in
      extend_by_pattern (S (bs, ty)) env tl
    | PatTuple es, TyTuple ts ->
      let new_env =
        List.fold2 es ts ~init:acc ~f:(fun acc e t -> extend_by_pattern (S (bs, t)) acc e)
      in
      (match new_env with
       | Ok env -> env
       | _ -> acc)
    | _ -> acc
  ;;

  let free_vars =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Map.find_exn map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TyVar n

let instantiate : scheme -> ty R.t =
  fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env id map =
  match Base.Map.find map id with
  | None -> fail (NoVariable id)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let infer_pattern =
  let rec (helper : TypeEnv.t -> pat -> (TypeEnv.t * ty) R.t) =
    fun env -> function
    | PatWild ->
      let* tv = fresh_var in
      return (env, tv)
    | PatNil ->
      let* tv = fresh_var in
      return (env, TyList tv)
    | PatLit l ->
      (match l with
       | LitInt _ -> return (env, TyLit "Int")
       | LitBool _ -> return (env, TyLit "Bool")
       | LitString _ -> return (env, TyLit "String")
       | LitChar _ -> return (env, TyLit "Char"))
    | PatVar x ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      return (env, tv)
    | PatTuple ps ->
      let rec infer_pattern_elements env pats types_acc =
        match pats with
        | [] -> return (env, List.rev types_acc)
        | p :: tl ->
          let* env', ty = helper env p in
          infer_pattern_elements env' tl (ty :: types_acc)
      in
      let* env', types = infer_pattern_elements env ps [] in
      return (env', TyTuple types)
    | PatCons (hd, tl) ->
      let* env1, typ1 = helper env hd in
      let* env2, typ2 = helper env1 tl in
      let* fresh = fresh_var in
      let* sub_uni = Subst.unify typ2 (TyList fresh) in
      let typ2 = Subst.apply sub_uni typ2 in
      let* sub3 = Subst.unify (TyList typ1) typ2 in
      let* final_sub = Subst.compose_all [ sub3; sub_uni ] in
      let env = TypeEnv.apply final_sub env2 in
      return (env, typ2)
  in
  helper
;;

let infer_expr =
  let rec (helper : TypeEnv.t -> expr -> (Subst.t * ty) R.t) =
    fun env -> function
    | ExprUnOp (op, e) ->
      let exp_ty =
        match op with
        | Neg -> TyLit "Int"
        | Not -> TyLit "Bool"
      in
      let* s, t = helper env e in
      let* subst = unify t exp_ty in
      let* final_subs = Subst.compose s subst in
      return (final_subs, exp_ty)
    | ExprBinOp (op, e1, e2) ->
      let* elem_ty, expr_ty =
        match op with
        | Add | Sub | Mul | Div -> return (TyLit "Int", TyLit "Int")
        | And | Or -> return (TyLit "Bool", TyLit "Bool")
        | Eq | Neq | Lt | Gt | Leq | Geq ->
          let* tv = fresh_var in
          return (tv, TyLit "Bool")
      in
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper env e2 in
      let* s3 = unify t1 elem_ty in
      let* s4 = unify (Subst.apply s1 t2) elem_ty in
      let* final_sub = Subst.compose_all [ s1; s2; s3; s4 ] in
      return (final_sub, expr_ty)
    | ExprVar x -> lookup_env x env
    | ExprFunc (x, e1) ->
      let* env2, tv = infer_pattern env x in
      let* s, ty = helper env2 e1 in
      let trez = TyArrow (Subst.apply s tv, ty) in
      return (s, trez)
    | ExprApp (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify t1 (TyArrow (t2, tv)) in
      let trez = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s1; s2; s3 ] in
      return (final_subst, trez)
    | ExprLit lit ->
      (match lit with
       | LitInt _ -> return (Subst.empty, TyLit "Int")
       | LitBool _ -> return (Subst.empty, TyLit "Bool")
       | LitString _ -> return (Subst.empty, TyLit "String")
       | LitChar _ -> return (Subst.empty, TyLit "Char"))
    | ExprIf (c, th, el) ->
      let* s1, t1 = helper env c in
      let* s2, t2 = helper env th in
      let* s3, t3 = helper env el in
      let* s4 = unify t1 (TyLit "Bool") in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s1; s2; s3; s4; s5 ] in
      R.return (final_subst, Subst.apply final_subst t2)
    | ExprTuple es ->
      let rec infer_elements env es subst_acc types_acc =
        match es with
        | [] -> return (subst_acc, types_acc)
        | e :: tl ->
          let* subst, ty = helper env e in
          let env' = TypeEnv.apply subst env in
          let* subst_acc' = Subst.compose subst subst_acc in
          infer_elements env' tl subst_acc' (ty :: types_acc)
      in
      let* final_subst, types = infer_elements env es Subst.empty [] in
      return (final_subst, TyTuple (List.rev types))
    | ExprNil ->
      let* tv = fresh_var in
      return (Subst.empty, TyList tv)
    | ExprCons (hd, tl) ->
      let* s2, t2 = helper env tl in
      let* s1, t1 = helper (TypeEnv.apply s2 env) hd in
      let* s3 = unify (TyList t1) t2 in
      let final_ty = Subst.apply s3 t2 in
      let* final_subst = Subst.compose_all [ s1; s2; s3 ] in
      return (final_subst, final_ty)
    | ExprLet (bindings, main_expr) ->
      let rec process_bindings env subst_acc = function
        | [] -> return (env, subst_acc)
        | (PatVar x, e) :: tl ->
          let* tv = fresh_var in
          let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
          let* s1, ty = helper env e in
          let scheme = generalize (TypeEnv.apply s1 env) ty in
          let env' = TypeEnv.extend (TypeEnv.apply s1 env) (x, scheme) in
          let* s_acc = Subst.compose s1 subst_acc in
          process_bindings env' s_acc tl
        | (x, _) :: tl ->
          let* env2, _ = infer_pattern env x in
          process_bindings env2 subst_acc tl
      in
      let* env', subst_acc = process_bindings env Subst.empty bindings in
      let* subst, ty = helper env' main_expr in
      let* final_subst = Subst.compose subst subst_acc in
      return (final_subst, ty)
    | ExprCase (expr, alts) ->
      let* e_subst, e_ty = helper env expr in
      let* tv = fresh_var in
      let* e_subst, e_ty, _ =
        Base.List.fold_left
          alts
          ~init:(return (e_subst, tv, e_ty))
          ~f:(fun acc (pat, e) ->
            let* subst, ty, c_ty = acc in
            let* pat_env, pat_ty = infer_pattern env pat in
            let* subst2 = unify c_ty pat_ty in
            let* subst3, e_ty = helper pat_env e in
            let* subst4 = unify ty e_ty in
            let* final_subst = Subst.compose_all [ subst; subst2; subst3; subst4 ] in
            return (final_subst, Subst.apply final_subst ty, Subst.apply final_subst c_ty))
      in
      return (e_subst, Subst.apply e_subst e_ty)
  in
  helper
;;

(* since we don't have 'rec' flag, we have to traverse AST *)
let rec is_recursive pat = function
  | ExprVar v -> v = pat
  | ExprLit _ -> false
  | ExprFunc (_, expr) -> is_recursive pat expr
  | ExprApp (e1, e2) -> is_recursive pat e1 || is_recursive pat e2
  | ExprIf (cond, e1, e2) ->
    is_recursive pat cond || is_recursive pat e1 || is_recursive pat e2
  | ExprTuple exprs -> List.exists (is_recursive pat) exprs
  | ExprCons (hd, tl) -> is_recursive pat hd || is_recursive pat tl
  | ExprNil -> false
  | ExprCase (e, alts) ->
    is_recursive pat e || List.exists (fun (_, expr) -> is_recursive pat expr) alts
  | ExprBinOp (_, e1, e2) -> is_recursive pat e1 || is_recursive pat e2
  | ExprUnOp (_, e) -> is_recursive pat e
  | ExprLet (bindings, e) ->
    List.exists (fun (_, binding_expr) -> is_recursive pat binding_expr) bindings
    || is_recursive pat e
;;

let infer_decl env (Ast.DeclLet (pat, expr)) =
  match pat with
  | PatVar v when is_recursive v expr ->
    let* tv = fresh_var in
    let env = TypeEnv.extend env (v, S (VarSet.empty, tv)) in
    let* s1, t1 = infer_expr env expr in
    let* s2 = unify (Subst.apply s1 tv) t1 in
    let* s = Subst.compose s2 s1 in
    let t2 = generalize env (Subst.apply s tv) in
    return (TypeEnv.extend env (v, t2))
  | _ ->
    let* env', _ = infer_pattern env pat in
    let* _, inferred_type = infer_expr env' expr in
    let scheme = generalize env' inferred_type in
    let env = TypeEnv.extend_by_pattern scheme env' pat in
    return env
;;

let rec infer_prog env = function
  | [] -> return env
  | decl :: rest ->
    let* env2 = infer_decl env decl in
    infer_prog env2 rest
;;

let run_prog p = run (infer_prog TypeEnv.empty p)

let pp_program fmt env =
  Base.Map.iteri env ~f:(fun ~key:v ~data:(S (_, ty)) ->
    Format.fprintf fmt "%s :: %a\n" v pp_type ty)
;;

let infer e =
  match run_prog e with
  | Ok ty -> Format.printf "%a" pp_program ty
  | Error err -> Format.printf "%a" pp_error err
;;
