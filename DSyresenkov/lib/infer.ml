(** Copyright 2021-2023, Ilya Syresenkov, Kakadu *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Typing

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
    val fold
      :  (int, 'a, Base.Int.comparator_witness) Base.Map.t
      -> init:'b t
      -> f:(int -> 'a -> 'b -> 'b t)
      -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
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
    let fold map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
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
    | TBase _ -> false
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple (ty1, ty2, tys) -> Base.List.exists (ty1 :: ty2 :: tys) ~f:(occurs_in v)
    | TList ty -> occurs_in v ty
  ;;

  let free_vars =
    let rec helper acc = function
      | TBase _ -> acc
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TTuple (ty1, ty2, tys) ->
        Base.List.fold_left
          (ty1 :: ty2 :: tys)
          ~f:(fun s t -> VarSet.union s (helper VarSet.empty t))
          ~init:acc
      | TList ty -> helper acc ty
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution *)
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

  type t = (fresh, ty, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let mapping k v =
    if Type.occurs_in k v then fail (OccursCheckFailed (k, v)) else return (k, v)
  ;;

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.update empty k ~f:(fun _ -> v))
  ;;

  let find_exn = Base.Map.find_exn
  let find = Base.Map.find
  let remove = Base.Map.remove

  let apply s =
    let rec helper = function
      | TBase b -> TBase b
      | TVar b ->
        (match find s b with
         | Some v -> v
         | None -> TVar b)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TTuple (ty1, ty2, tys) ->
        TTuple (helper ty1, helper ty2, Base.List.map tys ~f:helper)
      | TList ty -> TList (helper ty)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TBase l, TBase r when l = r -> return empty
    | TBase _, TBase _ -> fail (UnificationFailed (l, r))
    | TVar l, TVar r when l = r -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple (l1, l2, ls), TTuple (r1, r2, rs) ->
      if List.compare_lengths ls rs <> 0
      then fail (UnificationFailed (l, r))
      else
        Base.List.fold_left
          (Base.List.zip_exn (l1 :: l2 :: ls) (r1 :: r2 :: rs))
          ~f:(fun s (l, r) ->
            let* shead = unify l r in
            let* s = s in
            compose s shead)
          ~init:(return empty)
    | TList ty1, TList ty2 -> unify ty1 ty2
    | _ -> fail (UnificationFailed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Base.Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold s2 ~init:(return s1) ~f:extend

  let compose_all ss =
    Base.List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

module VarSet = struct
  include VarSet

  let fold_left set ~f ~init =
    VarSet.fold
      (fun x init ->
        let open R.Syntax in
        let* init = init in
        f init x)
      set
      init
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (s, t) -> (not (VarSet.mem v s)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (s, t) -> VarSet.diff (Type.free_vars t) s
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;
end

module TypeEnv = struct
  type t = (id, scheme, Base.String.comparator_witness) Base.Map.t

  let extend env (id, scheme) = Base.Map.set env ~key:id ~data:scheme
  let empty = Base.Map.empty (module Base.String)

  let free_vars =
    Base.Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Base.Map.map env ~f:(Scheme.apply s)
  let find_exn name map = Base.Map.find_exn map name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

let instantiate (S (set, ty)) =
  VarSet.fold_left
    set
    ~f:(fun ty name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s ty))
    ~init:(return ty)
;;

let generalize env ty =
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env id map =
  match Base.Map.find map id with
  | None -> fail (UndeclaredVariable id)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let infer_pattern =
  let rec helper : TypeEnv.t -> pattern -> (TypeEnv.t * ty) R.t =
    fun env -> function
    | PWild ->
      let* tv = fresh_var in
      return (env, tv)
    | PEmpty ->
      let* tv = fresh_var in
      return (env, TList tv)
    | PConst c ->
      (match c with
       | CInt _ -> return (env, TBase BInt)
       | CBool _ -> return (env, TBase BBool)
       | CUnit -> return (env, TBase BUnit))
    | PVar x ->
      (match Base.Map.find env x with
       | None ->
         let* tv = fresh_var in
         let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
         return (env, tv)
       | Some (S (_, ty)) -> return (env, ty))
    | PCons (p1, p2, ps) ->
      let p1, ps, plast =
        match List.rev ps with
        | [] -> p1, [], p2
        | h :: tl -> p1, p2 :: List.rev tl, h
      in
      let* env, ty1 = helper env p1 in
      let* env, ty =
        Base.List.fold_left
          ps
          ~init:(return (env, ty1))
          ~f:(fun acc p ->
            let* env, ty = acc in
            let* env, ty1 = helper env p in
            let* subst = unify ty ty1 in
            return (TypeEnv.apply subst env, Subst.apply subst ty))
      in
      let* env, ty_last = helper env plast in
      let* subst = unify (TList ty) ty_last in
      let ty_last = Subst.apply subst ty_last in
      let env = TypeEnv.apply subst env in
      return (TypeEnv.apply subst env, Subst.apply subst ty_last)
    (* | POr (p1, p2, ps) ->
       let* env, ty1 = helper env p1 in
       let* env, ty2 = helper env p2 in
       let* subst = unify ty1 ty2 in
       let* env, ty =
       Base.List.fold_left
       ps
       ~init:(return (env, Subst.apply subst ty1))
       ~f:(fun acc p ->
       let* env, ty = acc in
       let* env, ty1 = helper env p in
       let* subst = unify ty ty1 in
       return (TypeEnv.apply subst env, Subst.apply subst ty))
       in
       return (env, ty) *)
    | POr _ -> fail NotImplemented
  in
  helper
;;

let infer =
  let rec helper : TypeEnv.t -> expr -> (Subst.t * ty) R.t =
    fun env -> function
    | EConst c ->
      (match c with
       | CInt _ -> return (Subst.empty, TBase BInt)
       | CBool _ -> return (Subst.empty, TBase BBool)
       | CUnit -> return (Subst.empty, TBase BUnit))
    | EVar x -> lookup_env x env
    | EFun (x, e) ->
      let* tv = fresh_var in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s, ty = helper env2 e in
      let res_ty = TArrow (Subst.apply s tv, ty) in
      return (s, res_ty)
    | EBinop (op, l, r) ->
      let* l_subst, l_ty = helper env l in
      let* r_subst, r_ty = helper env r in
      (match op with
       | Eq | Neq | Les | Leq | Gre | Geq ->
         let* subst = unify l_ty r_ty in
         let* final_subst = Subst.compose_all [ l_subst; r_subst; subst ] in
         return (final_subst, TBase BBool)
       | _ ->
         let* subst1 = unify l_ty (TBase BInt) in
         let* subst2 = unify r_ty (TBase BInt) in
         let* final_subst = Subst.compose_all [ l_subst; r_subst; subst1; subst2 ] in
         return (final_subst, TBase BInt))
    | EApp (e1, e2) ->
      let* subst1, ty1 = helper env e1 in
      let* subst2, ty2 = helper (TypeEnv.apply subst1 env) e2 in
      let* tv = fresh_var in
      let* subst3 = unify (Subst.apply subst2 ty1) (TArrow (ty2, tv)) in
      let res_ty = Subst.apply subst3 tv in
      let* final_subst = Subst.compose_all [ subst1; subst2; subst3 ] in
      return (final_subst, res_ty)
    | ETuple (e1, e2, es) ->
      let* subst1, ty1 = helper env e1 in
      let* subst2, ty2 = helper env e2 in
      let* substs, tys =
        Base.List.fold_right
          es
          ~init:(return ([], []))
          ~f:(fun e acc ->
            let* subst, ty = helper env e in
            let* substs, tys = acc in
            return (subst :: substs, ty :: tys))
      in
      let* final_subst = Subst.compose_all (subst1 :: subst2 :: substs) in
      return (final_subst, TTuple (ty1, ty2, tys))
    | EList es ->
      (match es with
       | [] ->
         let* tv = fresh_var in
         return (Subst.empty, TList tv)
       | h :: tl ->
         let* final_subst, res_ty =
           Base.List.fold_left tl ~init:(helper env h) ~f:(fun acc e ->
             let* subst, ty = acc in
             let* subst1, ty1 = helper env e in
             let* subst2 = unify ty ty1 in
             let* final_subst = Subst.compose_all [ subst; subst1; subst2 ] in
             let res_ty = Subst.apply final_subst ty in
             return (final_subst, res_ty))
         in
         return (final_subst, TList res_ty))
    | EBranch (c, t, f) ->
      let* subst1, ty1 = helper env c in
      let* subst2, ty2 = helper env t in
      let* subst3, ty3 = helper env f in
      let* subst4 = unify ty1 (TBase BBool) in
      let* subst5 = unify ty2 ty3 in
      let* final_subst = Subst.compose_all [ subst1; subst2; subst3; subst4; subst5 ] in
      return (final_subst, Subst.apply subst5 ty3)
    | ELet (NonRec, _, e1, None) -> helper env e1
    | ELet (Rec, x, e1, None) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* subst1, ty1 = helper env e1 in
      let* subst2 = unify (Subst.apply subst1 tv) ty1 in
      let* final_subst = Subst.compose subst1 subst2 in
      return (final_subst, Subst.apply final_subst tv)
    | ELet (NonRec, x, e1, Some e2) ->
      let* subst1, ty1 = helper env e1 in
      let env2 = TypeEnv.apply subst1 env in
      let ty2 = generalize env2 ty1 in
      let env3 = TypeEnv.extend env2 (x, ty2) in
      let* subst2, ty3 = helper env3 e2 in
      let* final_subst = Subst.compose subst1 subst2 in
      return (final_subst, ty3)
    | ELet (Rec, x, e1, Some e2) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* subst1, ty1 = helper env e1 in
      let* subst2 = unify (Subst.apply subst1 tv) ty1 in
      let* subst = Subst.compose subst1 subst2 in
      let env = TypeEnv.apply subst env in
      let ty2 = generalize env (Subst.apply subst tv) in
      let* subst2, ty2 = helper TypeEnv.(extend (apply subst env) (x, ty2)) e2 in
      let* final_subst = Subst.compose subst subst2 in
      return (final_subst, ty2)
    | EMatch (c, cases) ->
      let* c_subst, c_ty = helper env c in
      let* tv = fresh_var in
      let* e_subst, e_ty =
        Base.List.fold_left
          cases
          ~init:(return (c_subst, tv))
          ~f:(fun acc (pat, e) ->
            let* subst, ty = acc in
            let* pat_env, pat_ty = infer_pattern env pat in
            let* subst2 = unify c_ty pat_ty in
            let* subst3, e_ty = helper pat_env e in
            let* subst4 = unify ty e_ty in
            let* final_subst = Subst.compose_all [ subst; subst2; subst3; subst4 ] in
            return (final_subst, Subst.apply final_subst ty))
      in
      let* final_subst = Subst.compose c_subst e_subst in
      return (final_subst, Subst.apply final_subst e_ty)
  in
  helper
;;

let run_infer e = Result.map snd (run (infer TypeEnv.empty e))

let check_program env program =
  let check_expr env e =
    let* _, ty = infer env e in
    match e with
    | ELet (_, x, _, None) ->
      let env = TypeEnv.extend env (x, S (VarSet.empty, ty)) in
      return (env, ty)
    | _ -> return (env, ty)
  in
  Base.List.fold_left program ~init:(return env) ~f:(fun env e ->
    let* env = env in
    let* env, _ = check_expr env e in
    return env)
;;

let typecheck env program = run (check_program env program)
