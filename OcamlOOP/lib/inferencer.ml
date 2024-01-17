(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typedtree
open Errors

(* A self-referncing object can only be represent using a keyword `self` (in contrast to OCaml) *)
let self_reference_alias = "self"

(*=======================References=======================
  - The solution is based on https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml?ref_type=heads
    - New features have been implemented and some errors in the code have been fixed
  - Type inference for objects is largely based on paper https://caml.inria.fr/pub/docs/u3-ocaml/ocaml-objects.html#fig/unification-ot:~:text=3.2.1%C2%A0%C2%A0Type%2Dchecking%20objects
*)

module R : sig
  include Base.Monad.Infix

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
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
    let ( let+ ) x f = x >>| f
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
    | _ -> false (* disable occurse check for object types *)
  ;;

  let vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TPrim _ -> acc
      | TList t -> helper acc t
      | TTuple ts -> List.fold_left (fun acc t -> helper acc t) acc ts
      | TField (_, t, ts) -> helper (helper acc t) ts
      | TNil -> acc
      | TObject (t, _, _) -> helper acc t
      | TPoly t -> VarSet.add t acc
    in
    helper VarSet.empty
  ;;
end

let tint = TPrim "int"
let tbool = TPrim "bool"
let tarrow l r = TArrow (l, r)
let tvar x = TVar x
let tlist x = TList x
let ttuple ts = TTuple ts
let tobject f v c = TObject (f, v, c)
let tfield n t ts = TField (n, t, ts)

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> tvar n
let fresh_poly = fresh >>| fun n -> TPoly n

module Subst : sig
  open Base

  type t = (fresh, ty, Int.comparator_witness) Map.t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open Base

  type t = (fresh, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let mapping k v =
    if Type.occurs_in k v then fail @@ occurs_check (k, v) else return (k, v)
  ;;

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
      | TObject (fields, vs, (C (b, _) as x)) ->
        let constr_binder =
          match find b s with
          | Some (TVar i) -> C (i, true)
          | _ -> x
        in
        let vs =
          List.fold_left vs ~init:[] ~f:(fun acc (name, t) -> (name, helper t) :: acc)
        in
        tobject (helper fields) (List.rev vs) constr_binder
      | TField (s, t, ts) -> TField (s, helper t, helper ts)
      | TPoly t as ty ->
        (match find t s with
         | None -> ty
         | Some (TVar i) -> TPoly i
         | Some x -> x)
      | other -> other
    in
    helper
  ;;

  let extract_anonymous_row =
    let empty = Map.empty (module String) in
    let rec helper fields = function
      | TField (k, t, ts) ->
        let fields = Map.update fields k ~f:(fun _ -> t) in
        helper fields ts
      | TPoly t -> return (fields, Some t)
      | _ -> return (fields, None)
    in
    helper empty
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TPrim _, TPrim _ -> fail (unification_failed (l, r))
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
       | _ -> fail (unification_failed (ttuple t1, ttuple t2)))
    | TNil, TNil -> return empty
    | TPoly i, t | t, TPoly i -> singleton i t
    | TObject (t1, _, _), TObject (t2, _, _) ->
      let* fields1, row1 = extract_anonymous_row t1 in
      let* fields2, row2 = extract_anonymous_row t2 in
      let common =
        Map.fold
          fields1
          ~init:(Map.empty (module String))
          ~f:(fun ~key ~data acc ->
            if Map.mem fields2 key then Map.update acc key ~f:(fun _ -> data) else acc)
      in
      let excl x = Map.filter_keys x ~f:(fun k -> not (Map.mem common k)) in
      let excl1 = excl fields1 in
      let excl2 = excl fields2 in
      let* subs =
        RMap.fold_left common ~init:(return empty) ~f:(fun k _ s ->
          let* s1 = unify (Map.find_exn fields1 k) (Map.find_exn fields2 k) in
          compose s1 s)
      in
      let construct_fields ~init fields =
        Map.fold fields ~init ~f:(fun ~key ~data fs -> TField (key, data, fs))
      in
      let* sub =
        match row1, Map.to_alist excl1, row2, Map.to_alist excl2 with
        | Some i, [], None, _ ->
          let expl_row = construct_fields excl2 in
          unify (TPoly i) (expl_row ~init:TNil)
        | None, _, Some i, [] ->
          let expl_row = construct_fields excl1 in
          unify (expl_row ~init:TNil) (TPoly i)
        | None, [], None, [] -> return empty
        | Some i1, _, Some i2, _ ->
          let* tv = fresh_poly in
          let fs1 = construct_fields excl1 ~init:tv in
          let fs2 = construct_fields excl2 ~init:tv in
          let* s1 = unify (TPoly i2) fs1 in
          let* s2 = unify (TPoly i1) fs2 in
          let* final_subs = compose s1 s2 in
          return final_subs
        | _ -> fail (unification_failed (t1, t2))
      in
      compose sub subs
    | _ -> fail (unification_failed (l, r))

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  and extend k v s =
    match find k s with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold_left s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2
  ;;

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env (v, scheme) = Map.update env v ~f:(fun _ -> scheme)

  let rec extend_by_pattern (S (bs, ty) as scheme) acc pat =
    match pat, ty with
    | Pat_var v, _ -> extend acc (v, scheme)
    | Pat_cons (h, tl), TList t ->
      let env = extend_by_pattern (S (bs, t)) acc h in
      extend_by_pattern (S (bs, ty)) env tl
    | Pat_tuple es, TTuple ts ->
      let new_env =
        List.fold2 es ts ~init:acc ~f:(fun acc e t -> extend_by_pattern (S (bs, t)) acc e)
      in
      (match new_env with
       | Ok env -> env
       | _ -> acc)
    | _ -> acc
  ;;

  let union env1 env2 =
    Map.fold env2 ~init:env1 ~f:(fun ~key ~data acc ->
      Map.update acc key ~f:(fun _ -> data))
  ;;

  let free_vars =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find xs name = Map.find xs name
end

let unify = Subst.unify

let instantiate (S (bs, t)) =
  VarSet.fold
    (fun name ty ->
      let* ty = ty in
      let* tv = fresh_var in
      let* s = Subst.singleton name tv in
      return (Subst.apply s ty))
    bs
    (return t)
;;

let generalize env ty is_rec ~pattern_name =
  let env =
    match is_rec, pattern_name with
    | Recursive, Some ident -> Base.Map.remove env ident
    | _ -> env
  in
  let free = VarSet.diff (Type.vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match TypeEnv.find xs e with
  | None -> fail (unbound_variable e)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pattern_vars =
  let rec helper acc = function
    | Pat_var v -> v :: acc
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
      then fail (several_bounds name)
      else return (name :: acc, env, ty))
;;

let const_infer fst = function
  | Const_int _ -> return (fst, tint)
  | Const_bool _ -> return (fst, tbool)
  | Const_nil ->
    let* tv = fresh_var in
    return (fst, tlist tv)
;;

let pattern_infer =
  let rec helper env = function
    | Pat_const c -> const_infer env c
    | Pat_var v ->
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
          ~init:(return (env, []))
          ~f:(fun (env, ts) pat ->
            let* env1, p1 = helper env pat in
            return (env1, p1 :: ts))
      in
      let* _, env, ty = check_several_bounds (pattern_vars pat) env (ttuple ty) in
      return (env, ty)
  in
  helper
;;

(* Introduce new type variable for each method in object *)
type first_pass_acc =
  { meths : string list (* for further construction of the object type *)
  ; meth_env : (string, ty, Base.String.comparator_witness) Base.Map.t
  }

type second_pass_acc =
  { vals : TypeEnv.t
  ; self_ty : ty
  ; subst : Subst.t
  }

let extract_method name = function
  | TObject (fs, _, _) as obj ->
    let rec find_meth = function
      | TField (s, t, ts) ->
        if String.equal name s then return (Subst.empty, t) else find_meth ts
      | TPoly _ as x ->
        let* tv = fresh_var in
        let* tv1 = fresh_poly in
        let* sub = unify x (TField (name, tv, tv1)) in
        return (sub, tv)
      | _ -> fail (undefined_method obj name)
    in
    find_meth fs
  | TVar _ as x ->
    let* tv = fresh_var in
    let* tv1 = fresh_poly in
    let* nc = fresh in
    let* sub = unify x (tobject (TField (name, tv, tv1)) [] (C (nc, false))) in
    return (sub, tv)
  | t -> fail (not_object t)
;;

let object_infer env ~infer_expr { o_self; o_fields } =
  let open Base in
  let* self_opaque =
    match o_self with
    | Pat_any -> return true
    | Pat_var name when String.equal name self_reference_alias -> return false
    | _ -> fail cannot_match_self
  in
  let add_val k v = function
    | TObject (t, vals, c) ->
      return (tobject t (List.Assoc.add vals ~equal:String.equal k v) c)
    | t -> fail (not_object t)
  in
  let first_field_pass acc =
    let { meths; meth_env } = acc in
    function
    | Obj_val _ -> return acc
    | Obj_method (_, name, _exp) ->
      if Map.mem meth_env name
      then fail (multiple_method name)
      else
        let* tv = fresh_var in
        let meth_env = Map.update meth_env name ~f:(fun _ -> tv) in
        let meths = name :: meths in
        return { meths; meth_env }
  in
  let* { meths; meth_env } =
    RList.fold_left
      o_fields
      ~init:(return { meth_env = Base.Map.empty (module String); meths = [] })
      ~f:first_field_pass
  in
  let self =
    List.fold_left meths ~init:TNil ~f:(fun ts t ->
      match TypeEnv.find meth_env t with
      | None -> ts
      | Some ty -> tfield t ty ts)
  in
  let option_eq_ty k t subs =
    let cmp = Option.equal equal_ty in
    cmp (Subst.find k subs) t
  in
  (* Determine whether the object type is recursive and, if necessary, apply type sharing *)
  let bind_constr m_fp m_sp self s =
    match self, m_sp, m_fp with
    | TObject (_, _, C (b, false)), (TVar _ as ty), TVar i1
      when option_eq_ty b (Some ty) s ->
      let* sub = Subst.singleton i1 ty in
      let+ final_subs = Subst.compose sub s in
      Subst.apply final_subs self
    | TObject (_, _, C (b, true)), TVar i, TVar i1 when i = b ->
      let+ sub = Subst.singleton i1 (tvar b) in
      Subst.apply sub self
    | _ ->
      let* sub = unify m_fp m_sp in
      let+ sub1 = Subst.compose s sub in
      Subst.apply sub1 self
  in
  let second_field_pass acc =
    let { vals; self_ty; subst } = acc in
    function
    | Obj_val (name, e) ->
      if Map.mem vals name
      then fail (multiple_variable name)
      else
        let* sub, ty = infer_expr env e in
        let vals = TypeEnv.extend vals (name, S (VarSet.empty, ty)) in
        let* subst = Subst.compose sub subst in
        let* self_ty = add_val name ty self_ty in
        return { self_ty; vals; subst }
    | Obj_method (_, name, e) ->
      let env1 = TypeEnv.union env vals in
      let env2 =
        if self_opaque
        then env1
        else TypeEnv.extend env1 (self_reference_alias, S (VarSet.empty, self_ty))
      in
      let* s, m_sp = infer_expr env2 e in
      let* _, m_fp = extract_method name self_ty in
      let* self_ty = bind_constr m_fp m_sp self_ty s in
      let* final_subs = Subst.compose subst s in
      return { acc with self_ty; subst = final_subs }
  in
  let* nc = fresh in
  let* { self_ty; subst } =
    RList.fold_left
      o_fields
      ~init:
        (return
           { vals = TypeEnv.empty
           ; self_ty = tobject self [] (C (nc, false))
           ; subst = Subst.empty
           })
      ~f:second_field_pass
  in
  return (subst, self_ty)
;;

let infer =
  let rec helper env = function
    | Exp_constant c -> const_infer Subst.empty c
    | Exp_ident v -> lookup_env v env
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
    | Exp_apply (e, e') ->
      let* tv = fresh_var in
      let* s1, t1 = helper env e in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e' in
      let* sub = unify (tarrow t2 tv) (Subst.apply s2 t1) in
      let* final_subs = Subst.compose_all [ s1; s2; sub ] in
      let trez = Subst.apply final_subs tv in
      return (final_subs, trez)
    | Exp_function (v, e) ->
      let* env', t = pattern_infer env v in
      let* s, t' = helper env' e in
      let ty = tarrow (Subst.apply s t) t' in
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
      let scheme = generalize (TypeEnv.apply s1 env) t1 Nonrecursive ~pattern_name:None in
      let* env1, t2 = pattern_infer env d_pat in
      let env2 = TypeEnv.extend_by_pattern scheme env1 d_pat in
      let* sub = unify t2 t1 in
      let* sub1 = Subst.compose sub s1 in
      let env3 = TypeEnv.apply sub1 env2 in
      let* s2, t2 = helper env3 e in
      let* final_subs = Subst.compose_all [ sub1; s2 ] in
      return (final_subs, t2)
    | Exp_let ({ d_rec = Recursive; d_pat; d_expr }, e) ->
      (match d_pat with
       | Pat_var v ->
         let* tv = fresh_var in
         let env1 = TypeEnv.extend env (v, S (VarSet.empty, tv)) in
         let* s1, t1 = helper env1 d_expr in
         let* s2 = unify (Subst.apply s1 tv) t1 in
         let* s = Subst.compose s2 s1 in
         let env = TypeEnv.apply s env1 in
         let t2 = generalize env (Subst.apply s tv) Recursive ~pattern_name:(Some v) in
         let* s2, t2 = helper TypeEnv.(extend env (v, t2)) e in
         let* final_subs = Subst.compose s s2 in
         return (final_subs, t2)
       | _ -> fail no_variable_rec)
    | Exp_match (match_exp, cases) ->
      let* s, tcond = helper env match_exp in
      let env = TypeEnv.apply s env in
      let* tv = fresh_var in
      RList.fold_left
        cases
        ~init:(return (s, tv))
        ~f:(fun (s, t) (p, e) ->
          let* env, tp = pattern_infer env p in
          let* s' = unify tcond tp in
          let* sub, te = helper env e in
          let* sub' = unify t te in
          let* final_subs = Subst.compose_all [ sub'; sub; s'; s ] in
          return (final_subs, Subst.apply final_subs t))
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
    | Exp_send (e, meth) ->
      let* s, t = helper env e in
      let* sub, meth_ty = extract_method meth t in
      let* meth_ty =
        match t, meth_ty with
        | (TObject (_, _, C (b1, true)) as self_ty), ty ->
          let* sub = Subst.singleton b1 self_ty in
          return (Subst.apply sub ty)
        | _ -> return meth_ty
      in
      let* final_subs = Subst.compose s sub in
      return (final_subs, meth_ty)
    | Exp_object o -> object_infer env ~infer_expr:helper o
    | Exp_override bindings ->
      (match TypeEnv.find env self_reference_alias with
       | Some (S (_, TObject (_, vals, C (binder, presence)))) ->
         let* sub =
           RList.fold_left bindings ~init:(return Subst.empty) ~f:(fun subs (v, e) ->
             match Base.List.Assoc.find vals v ~equal:String.equal with
             | Some t ->
               let* s1, t1 = helper env e in
               let* sub1 = unify t t1 in
               let* final_subs = Subst.compose_all [ sub1; s1; subs ] in
               return final_subs
             | None -> fail (unbound_variable v))
         in
         if presence
         then return (sub, tvar binder)
         else
           let* tv = fresh in
           let* s = Subst.singleton binder (TVar tv) in
           let* final_subs = Subst.compose s sub in
           return (final_subs, tvar tv)
       | _ -> fail (unbound_variable self_reference_alias))
  in
  helper
;;

let value_type env = function
  | { d_rec = Nonrecursive; d_pat; d_expr } ->
    let* s1, t1 = infer env d_expr in
    let scheme = generalize (TypeEnv.apply s1 env) t1 Nonrecursive ~pattern_name:None in
    let* env1, t2 = pattern_infer env d_pat in
    let env2 = TypeEnv.extend_by_pattern scheme env1 d_pat in
    let* sub = unify t1 t2 in
    let* sub1 = Subst.compose s1 sub in
    let env3 = TypeEnv.apply sub1 env2 in
    return env3
  | { d_rec = Recursive; d_pat; d_expr } ->
    (match d_pat with
     | Pat_var v ->
       let* tv = fresh_var in
       let env = TypeEnv.extend env (v, S (VarSet.empty, tv)) in
       let* s1, t1 = infer env d_expr in
       let* s2 = unify (Subst.apply s1 tv) t1 in
       let* s = Subst.compose s2 s1 in
       let env = TypeEnv.apply s env in
       let t2 = generalize env (Subst.apply s tv) Recursive ~pattern_name:(Some v) in
       return (TypeEnv.extend env (v, t2))
     | _ -> fail no_variable_rec)
;;

let check_program program =
  let env = TypeEnv.empty in
  let helper env =
    RList.fold_left program ~init:(return env) ~f:(fun env ->
        function
        | Str_eval e ->
          let* _, _ = infer env e in
          return env
        | Str_value d -> value_type env d)
  in
  run (helper env)
;;

module PP = struct
  (* Convert binders for further replacement with strings
     Example: type '10 * '22 -> '11 will be replaced by '0 * '1 -> '2 *)
  let reconstruct_binders ty =
    let open Base in
    let empty = Map.empty (module Int) in
    (* k - old binder, v - new binder*)
    let find_insert (k, v) binders =
      match Map.find binders k with
      | Some _ -> v, binders
      | None -> v + 1, Map.update binders k ~f:(fun _ -> v)
    in
    let repls =
      let rec helper (last, acc) = function
        | TVar n -> find_insert (n, last) acc
        | TArrow (l, r) ->
          let last1, acc1 = helper (last, acc) l in
          helper (last1, acc1) r
        | TList t -> helper (last, acc) t
        | TTuple ts -> List.fold_left ts ~init:(last, acc) ~f:helper
        | TObject (t, _, C (_, false)) -> helper (last, acc) t
        | TObject (t, _, C (constr, true)) ->
          let last1, acc1 = find_insert (constr, last) acc in
          helper (last1, acc1) t
        | TField (_, t, ts) ->
          let last1, acc1 = helper (last, acc) t in
          helper (last1, acc1) ts
        | TPrim _ | TNil | TPoly _ -> last, acc
      in
      snd @@ helper (0, empty) ty
    in
    let rec construct = function
      | TVar n -> tvar (Map.find_exn repls n)
      | TArrow (l, r) -> tarrow (construct l) (construct r)
      | TList t -> tlist (construct t)
      | TTuple ts -> ttuple (List.map ts ~f:construct)
      | TObject (t, v, (C (_, false) as constr)) -> tobject (construct t) v constr
      | TObject (t, v, C (constr, true)) ->
        tobject (construct t) v (C (Map.find_exn repls constr, true))
      | TField (n, t, ts) -> tfield n (construct t) (construct ts)
      | other -> other
    in
    construct ty
  ;;

  let convert_to_string binder =
    let rec helper binder acc =
      if binder < 26
      then (
        let s = Char.chr (97 + binder) in
        Base.Char.to_string s ^ acc)
      else (
        let tl = Char.chr (97 + (binder mod 26)) in
        helper ((binder / 26) - 1) (Format.sprintf "%c%s" tl acc))
    in
    helper binder ""
  ;;

  let pp_type ppf ty =
    let new_ty = reconstruct_binders ty in
    let open Format in
    let rec helper ppf = function
      | TVar n, _ -> fprintf ppf "'%s" (convert_to_string n)
      | TPrim s, _ -> pp_print_string ppf s
      | TArrow (l, r), pos ->
        let arrow ppf pos =
          match l with
          | TArrow (_, _) -> fprintf ppf "(%a) -> %a" helper (l, false) helper (r, pos)
          | _ -> fprintf ppf "%a -> %a" helper (l, pos) helper (r, pos)
        in
        if pos then fprintf ppf "(%a)" arrow pos else fprintf ppf "%a" arrow pos
      | TList t, pos -> fprintf ppf "%a list" helper (t, pos)
      | TTuple ts, pos ->
        let pp_tuple ppf (ts, pos) =
          fprintf
            ppf
            "%a"
            (pp_print_list
               ~pp_sep:(fun ppf () -> fprintf ppf " * ")
               (fun ppf t -> fprintf ppf "%a" helper (t, pos)))
            ts
        in
        if pos
        then fprintf ppf "(%a)" pp_tuple (ts, pos)
        else fprintf ppf "%a" pp_tuple (ts, true)
      | TObject (t, _, C (b, presence)), pos ->
        let constr_displ =
          if presence then sprintf " as '%s" (convert_to_string b) else ""
        in
        fprintf ppf "< %a >%s" helper (t, pos) constr_displ
      | TField (name, t, TNil), pos -> fprintf ppf "%s : %a" name helper (t, pos)
      | TField (name, t, ts), pos ->
        fprintf ppf "%s : %a; %a" name helper (t, pos) helper (ts, pos)
      | TPoly _, _ -> fprintf ppf ".."
      | TNil, _ -> ()
    in
    helper ppf (new_ty, false)
  ;;

  let pp_program ppf env =
    Base.Map.iteri env ~f:(fun ~key:v ~data:(S (_, ty)) ->
      Format.fprintf ppf "var %s: %a\n" v pp_type ty)
  ;;

  let map_binder t1 t2 binder =
    let rec helper acc = function
      | TVar n1, TVar n2 -> if n1 = binder then convert_to_string n2 else acc
      | TArrow (l1, r1), TArrow (l2, r2) ->
        let acc = helper acc (l1, l2) in
        helper acc (r1, r2)
      | TList t1, TList t2 -> helper acc (t1, t2)
      | TTuple ts1, TTuple ts2 -> List.fold_left helper acc (List.combine ts1 ts2)
      | _ -> acc
    in
    helper "" (t1, t2)
  ;;

  let pp_error ppf = function
    | Occurs_check (k, v) ->
      let v1 = reconstruct_binders v in
      let literal = map_binder v v1 k in
      Format.fprintf ppf "The type variable '%s occurs inside %a" literal pp_type v
    | Unbound_variable s -> Format.fprintf ppf "Unbound value '%s'" s
    | Unification_failed (l, r) ->
      Format.fprintf
        ppf
        "This expression has type %a but an expression was expected of type %a"
        pp_type
        l
        pp_type
        r
    | Several_bounds s -> Format.fprintf ppf "Variable %s is bound several times" s
    | No_variable_rec ->
      Format.fprintf ppf "Only variables are allowed as left-side of 'let rec'"
    | Multiple_definition (kind, name) ->
      let msg =
        match kind with
        | Variable -> "instance variable"
        | Method -> "method"
      in
      Format.fprintf ppf "The %s `%s' has multiple definitions in object" msg name
    | Undefined_method (t, n) ->
      Format.fprintf ppf "This object has type %a \n It has not method %s" pp_type t n
    | Not_object t ->
      Format.fprintf ppf "Expression is not an object; it has type %a" pp_type t
    | Cannot_match_self -> Format.fprintf ppf "This pattern cannot match self"
  ;;
end
