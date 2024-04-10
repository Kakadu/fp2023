(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html *)

(** https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml?ref_type=heads *)

open Typing

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

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

  (** stops the computation at the first error.
      p >>= f run p, pass its result to f, run the f, return the result of f *)
  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun monad f state ->
    let final_state, result = monad state in
    match result with
    | Error err -> final_state, Error err
    | Ok v -> f v final_state
  ;;

  (** stops the computation at the first error.
      p >>| f run p, if it succeeds with result v, will return f v *)
  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun v f state ->
    match v state with
    | state, Ok v -> state, Ok (f v)
    | state, Error err -> state, Error err
  ;;

  (** either a success (Ok value) or a failure (Error err) *)
  let return v last = last, Base.Result.return v

  let bind v ~f = v >>= f
  let fail error state = state, Base.Result.fail error

  module Syntax = struct
    let ( let* ) v f = bind v ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc -> acc >>= fun acc -> f key data acc)
    ;;
  end

  module RList = struct
    let fold_left lt ~init ~f =
      Base.List.fold_left lt ~init ~f:(fun acc item -> acc >>= fun acc -> f acc item)
    ;;
  end

  (** creating new type var *)
  let fresh : int t = fun last -> last + 1, Result.Ok last

  (** run from initial state of 0 and extract the second component of the resulting tuple *)
  let run monad = snd (monad 0)
end

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar x -> x = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TList typ -> occurs_in v typ
    | TTuple typ_list -> List.exists (occurs_in v) typ_list
    | TPrim _ -> false
  ;;

  let type_vars_acc =
    let rec helper acc = function
      | TVar x -> VarSet.add x acc
      | TArr (l, r) -> helper (helper acc l) r
      | TList typ -> helper acc typ
      | TTuple typ_list -> List.fold_left helper acc typ_list
      | TPrim _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  type t = (int, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let singleton key v =
    if Type.occurs_in key v
    then fail OccursCheck
    else return (Base.Map.singleton (module Base.Int) key v)
  ;;

  let find sub key = Base.Map.find sub key
  let remove sub key = Base.Map.remove sub key

  let apply sub =
    let rec helper typ =
      match typ with
      | TVar n -> find sub n |> Option.value ~default:(tvar n)
      | TArr (l, r) -> tarrow (helper l) (helper r)
      | TList typ -> tlist (helper typ)
      | TTuple t_list -> ttuple (Base.List.map t_list ~f:helper)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when l = r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar a, t | t, TVar a -> singleton a t
    | TArr (l1, r1), TArr (l2, r2) -> unify_arr l1 r1 l2 r2
    | _ -> fail (UnificationFailed (l, r))

  and unify_arr l1 r1 l2 r2 =
    let* sub1 = unify l1 l2 in
    let* sub2 = unify (apply sub1 r1) (apply sub1 r2) in
    compose sub1 sub2

  and extend k v sub =
    match find sub k with
    | None ->
      let v = apply sub v in
      let* sub2 = singleton k v in
      Base.Map.fold sub ~init:(return sub2) ~f:(fun ~key ~data acc ->
        let* acc = acc in
        let new_data = apply sub2 data in
        return (Base.Map.update acc key ~f:(fun _ -> new_data)))
    | Some value ->
      let* sub2 = unify v value in
      compose sub sub2

  and compose sub1 sub2 = RMap.fold_left sub2 ~init:(return sub1) ~f:extend

  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
end

module Scheme = struct
  let free_vars (Scheme (bind_vars, ty)) = VarSet.diff (Type.type_vars_acc ty) bind_vars

  (** check whether a type variable occurs in a type scheme *)
  let occurs_in tvar (Scheme (bind_vars, ty)) =
    (not (VarSet.mem tvar bind_vars)) && Type.occurs_in tvar ty
  ;;

  (** apply a substitution to a type scheme *)
  let apply sub (Scheme (bind_vars, ty)) =
    let sub' = VarSet.fold (fun sub key -> Subst.remove key sub) bind_vars sub in
    Scheme (bind_vars, Subst.apply sub' ty)
  ;;
end

module TypeEnv = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  (** calculates the set of free type vars in a given type env *)
  let free_vars env =
    Base.Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let extend : t -> string -> scheme -> t =
    fun env key scheme -> Base.Map.update env key ~f:(fun _ -> scheme)
  ;;

  let apply env sub = Base.Map.map env ~f:(Scheme.apply sub)
  let find env key = Base.Map.find env key
  let update map key v = Base.Map.update map key ~f:(fun _ -> v)
end

open R
open R.Syntax

let fresh_var = fresh >>| fun x -> tvar x

(** a type is brought into existence *)
let instantiate (Scheme (bind_vars, ty)) =
  let fold_acc var_name acc =
    let* acc = acc in
    let* fv = fresh_var in
    let* sub = Subst.singleton var_name fv in
    return (Subst.apply sub acc)
  in
  VarSet.fold fold_acc bind_vars (return ty)
;;

(* creating a scheme out of a type *)
let generalize env ty =
  let free = VarSet.diff (Type.type_vars_acc ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

open Ast

let infer_const = function
  | Ast.Int _ -> tint
  | Ast.Bool _ -> tbool
  | Ast.Char _ -> tchar
  | Ast.String _ -> tstring
  | Ast.Unit -> tunit
;;

let infer_pattern env = function
  | Ast.PAny | Ast.PNil -> fresh_var >>| fun fv -> Subst.empty, fv, env
  | Ast.PConst c ->
    let fv = infer_const c in
    return (Subst.empty, fv, env)
  | Ast.PVar x ->
    fresh_var
    >>| fun fv ->
    let env' = TypeEnv.extend env x (Scheme (VarSet.empty, fv)) in
    Subst.empty, fv, env'
;;

let infer_binop_type = function
  | Eq | Neq | Gt | Gtq | Lt | Ltq -> fresh_var >>| fun fv -> fv, tbool
  | Plus | Minus | Mult | Div | Mod -> return (tint, tint)
  | And | Or -> return (tbool, tbool)
;;

let infer_expr =
  let rec helper env = function
    | EConst x -> return (Subst.empty, infer_const x)
    | EVar x ->
      (match TypeEnv.find env x with
       | Some scheme ->
         let* ans = instantiate scheme in
         return (Subst.empty, ans)
       | None -> fail @@ UndeclaredVariable x)
    | EBinOp (op, e1, e2) ->
      let* args_type, expr_type = infer_binop_type op in
      let* sub_l, ty1 = helper env e1 in
      let* sub_r, ty2 = helper env e2 in
      let* sub1 = Subst.unify ty1 args_type in
      let* sub2 = Subst.unify (Subst.apply sub1 ty2) args_type in
      let* sub = Subst.compose_all [ sub_l; sub_r; sub1; sub2 ] in
      return (sub, expr_type)
    | EApp (e1, e2) ->
      let* sub1, ty1 = helper env e1 in
      let* sub2, ty2 = helper (TypeEnv.apply env sub1) e2 in
      let* fv = fresh_var in
      let* sub3 = Subst.unify (Subst.apply sub2 ty1) (tarrow ty2 fv) in
      let ty = Subst.apply sub3 fv in
      let* sub = Subst.compose_all [ sub1; sub2; sub3 ] in
      return (sub, ty)
    | EFun (pattern, e) ->
      let* _, t1, env' = infer_pattern env pattern in
      let* sub2, t2 = helper env' e in
      let ty = tarrow (Subst.apply sub2 t1) t2 in
      return (sub2, ty)
    | EIfThenElse (e1, e2, e3) ->
      let* sub_e1, t1 = helper env e1 in
      let* sub_e2, t2 = helper env e2 in
      let* sub_e3, te = helper env e3 in
      let* sub = Subst.unify t1 tbool in
      let* fv = fresh_var in
      let* sub1 = Subst.unify fv t2 in
      let* sub2 = Subst.unify fv te in
      let* final_subs = Subst.compose_all [ sub_e1; sub_e2; sub_e3; sub; sub1; sub2 ] in
      return (final_subs, Subst.apply final_subs t2)
    | ELet ((NonRec, _, e1), EUnit) -> helper env e1
    | ELet ((Rec, x, e1), EUnit) ->
      let* fv = fresh_var in
      let env = TypeEnv.extend env x (Scheme (VarSet.empty, fv)) in
      let* sub1, ty1 = helper env e1 in
      let* sub2 = Subst.unify (Subst.apply sub1 fv) ty1 in
      let* sub = Subst.compose sub1 sub2 in
      return (sub, Subst.apply sub fv)
    | _ -> return (Subst.empty, tint)
  in
  helper
;;

let run_inference expr = Result.map snd (run (infer_expr TypeEnv.empty expr))

(** module for test typing *)
module PP = struct
  open Format

  let rec pp_type ppf (typ : typ) =
    match typ with
    | TPrim x ->
      (match x with
       | Int -> fprintf ppf "int"
       | Bool -> fprintf ppf "bool"
       | Unit -> fprintf ppf "unit"
       | Char -> fprintf ppf "char"
       | String -> fprintf ppf "string")
    | TVar x -> fprintf ppf "%s" @@ Char.escaped (Char.chr (x + 97))
    | TArr (l, r) ->
      (match l, r with
       | TArr _, _ -> fprintf ppf "(%a) -> %a" pp_type l pp_type r
       | _, _ -> fprintf ppf "%a -> %a" pp_type l pp_type r)
    | _ -> fprintf ppf "not implemented"
  ;;

  let pp_error ppf (err : error) =
    match err with
    | OccursCheck -> fprintf ppf "Occurs check failed"
    | UndeclaredVariable s -> fprintf ppf "Undefined variable: %s" s
    | UnificationFailed (l, r) ->
      fprintf ppf "Unification failed on %a and %a" pp_type l pp_type r
    | NotImplemented -> fprintf ppf "NotImplemented"
  ;;

  let print_typ typ = printf "%a\n" pp_type typ
  let print_type_error error = printf "%a\n" pp_error error

  let print_result expr =
    match run_inference expr with
    | Ok typ -> print_typ typ
    | Error x -> print_type_error x
  ;;
end
