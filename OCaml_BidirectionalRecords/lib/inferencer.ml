(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html *)
(* https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml/inferencer.ml?ref_type=heads *)

(** 
    TODO:
    - write full miniML task
          - tuples
          - list
          - cons
    - Rec & NonRec let 
    - !! refactor
    - pp module
*)

open Typing

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
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

  (** stops the computation at the first error *)
  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun monad f state ->
    let last, result = monad state in
    match result with
    | Error err -> last, Error err
    | Ok v -> f v last
  ;;

  (** ignores errors and continues with the computation *)
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
      let open Syntax in
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    let fold_left lt ~init ~f =
      let open Syntax in
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last

  (** run from initial state of 0 and extract the second component of the resulting tuple *)
  let run monad = snd (monad 0)
end

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TList typ -> occurs_in v typ
    | TTuple typ_list ->
      List.fold_left (fun acc item -> acc || occurs_in v item) false typ_list
    | TPrim _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArr (l, r) -> helper (helper acc l) r
      | TList typ -> helper acc typ
      | TTuple typ_list -> List.fold_left (fun acc item -> helper acc item) acc typ_list
      | TPrim _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val find : t -> int -> typ option
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
    then fail `OccursCheck
    else return (Base.Map.singleton (module Base.Int) key v)
  ;;

  let find sub key = Base.Map.find sub key
  let remove sub key = Base.Map.remove sub key

  let apply sub =
    let rec helper = function
      | TVar n ->
        (match find sub n with
         | None -> tvar n
         | Some v -> v)
      | TArr (left, right) -> tarrow (helper left) (helper right)
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
    | TList typ1, TList typ2 -> unify typ1 typ2
    | TArr (l1, r1), TArr (l2, r2) ->
      let* sub1 = unify l1 l2 in
      let* sub2 = unify (apply sub1 r1) (apply sub1 r2) in
      compose sub1 sub2
    | TTuple t_list1, TTuple t_list2 ->
      (match
         Base.List.fold2 t_list1 t_list2 ~init:(return empty) ~f:(fun acc it1 it2 ->
           let* sub1 = acc in
           let* sub2 = unify (apply sub1 it1) (apply sub1 it2) in
           compose sub1 sub2)
       with
       | Ok r -> r
       | _ -> fail (`UnificationFailed (l, r)))
    | _ -> fail (`UnificationFailed (l, r))

  and extend k v sub =
    match find sub k with
    | None ->
      let v = apply sub v in
      let* sub2 = singleton k v in
      let f1 ~key ~data acc =
        let* acc = acc in
        let new_data = apply sub2 data in
        return (Base.Map.update acc key ~f:(fun _ -> new_data))
      in
      Base.Map.fold sub ~init:(return sub2) ~f:f1
    | Some vl ->
      let* sub2 = unify v vl in
      compose sub sub2

  and compose sub1 sub2 = RMap.fold_left sub2 ~init:(return sub1) ~f:extend

  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
end

module Scheme = struct
  let free_vars = function
    | Scheme (bind_vars, ty) -> VarSet.diff (Type.type_vars ty) bind_vars
  ;;

  (** check whether a type variable occurs in a type scheme *)
  let occurs_in tvar = function
    | Scheme (bind_vars, ty) ->
      (not (VarSet.mem tvar bind_vars)) && Type.occurs_in tvar ty
  ;;

  (** apply a substitution to a type scheme *)
  let apply sub = function
    | Scheme (bind_vars, ty) ->
      let sub2 = VarSet.fold (fun sub key -> Subst.remove key sub) bind_vars sub in
      Scheme (bind_vars, Subst.apply sub2 ty)
  ;;
end

module TypeEnv = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  let free_vars : t -> VarSet.t =
    fun env ->
    Base.Map.fold
      ~init:VarSet.empty
      ~f:(fun ~key:_ ~data acc -> VarSet.union acc (Scheme.free_vars data))
      env
  ;;

  let extend : t -> string -> scheme -> t =
    fun env key schema -> Base.Map.update env key ~f:(fun _ -> schema)
  ;;

  let apply : t -> Subst.t -> t = fun env sub -> Base.Map.map env ~f:(Scheme.apply sub)
  let find env key = Base.Map.find env key

  (* overwrite existing *)
  let update mp k v = Base.Map.update mp k ~f:(function _ -> v)
end

open R
open R.Syntax

let fresh_var = fresh >>| fun x -> tvar x

(** a type is brought into existence *)
let instantiate : scheme -> typ R.t =
  fun (Scheme (bind_var, ty)) ->
  VarSet.fold
    (fun var_name acc ->
      let* acc = acc in
      let* fv = fresh_var in
      let* sub = Subst.singleton var_name fv in
      return (Subst.apply sub acc))
    bind_var
    (return ty)
;;

(* creating a scheme out of a type *)
let generalize env ty =
  let free = VarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
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

let rec infer_pattern env = function
  | Ast.PAny ->
    let* fresh = fresh_var in
    return (Subst.empty, fresh, env)
  | Ast.PNil ->
    let* fresh = fresh_var in
    return (Subst.empty, fresh, env)
  | Ast.PConst c ->
    let fresh = infer_const c in
    return (Subst.empty, fresh, env)
  | Ast.PVar x ->
    let* fresh = fresh_var in
    let env' = TypeEnv.extend env x (Scheme (VarSet.empty, fresh)) in
    return (Subst.empty, fresh, env')
  | _ -> fail `AddLater
;;

let binop_type operator =
  match operator with
  | Eq | Neq | Gt | Gtq | Lt | Ltq ->
    let* fv = fresh_var in
    return (fv, tbool)
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
       | None -> fail @@ `UndeclaredVariable x)
    | EBinOp (op, e1, e2) ->
      let* args_type, expr_type = binop_type op in
      let* sub_left, ty1 = helper env e1 in
      let* sub_right, ty2 = helper env e2 in
      let* sub1 = Subst.unify ty1 args_type in
      let* sub2 = Subst.unify (Subst.apply sub1 ty2) args_type in
      let* sub = Subst.compose_all [ sub_left; sub_right; sub1; sub2 ] in
      return (sub, expr_type)
    | EApp (e1, e2) ->
      let* subst1, ty1 = helper env e1 in
      let* subst2, ty2 = helper (TypeEnv.apply env subst1) e2 in
      let* tv = fresh_var in
      let* subst3 = Subst.unify (Subst.apply subst2 ty1) (tarrow ty2 tv) in
      let res_ty = Subst.apply subst3 tv in
      let* final_subst = Subst.compose_all [ subst1; subst2; subst3 ] in
      return (final_subst, res_ty)
    | EFun (pattern, e) ->
      let* sub1, t1, env' = infer_pattern env pattern in
      let* sub2, t2 = helper env' e in
      let ty = tarrow (Subst.apply sub2 t1) t2 in
      return (sub2, ty)
    | EIfThenElse (e1, e2, e3) ->
      let* si, ti = helper env e1 in
      let* st, tt = helper env e2 in
      let* se, te = helper env e3 in
      let* sub = Subst.unify ti tbool in
      let* tv = fresh_var in
      let* sub1 = Subst.unify tv tt in
      let* sub2 = Subst.unify tv te in
      let* final_subs = Subst.compose_all [ si; st; se; sub; sub1; sub2 ] in
      return (final_subs, Subst.apply final_subs tt)
    | ELet ((_, x, e1), EUnit) ->
      let* tv = fresh_var in
      let env = TypeEnv.extend env x (Scheme (VarSet.empty, tv)) in
      let* subst1, ty1 = helper env e1 in
      let* subst2 = Subst.unify (Subst.apply subst1 tv) ty1 in
      let* final_subst = Subst.compose subst1 subst2 in
      return (final_subst, Subst.apply final_subst tv)
    | _ -> return (Subst.empty, tint)
  in
  helper
;;

let run_inference expr = Result.map snd (run (infer_expr TypeEnv.empty expr))

let rec pp_type ppf (typ : typ) =
  match typ with
  | TPrim x ->
    (match x with
     | Int -> Format.fprintf ppf "int"
     | Bool -> Format.fprintf ppf "bool"
     | Unit -> Format.fprintf ppf "unit"
     | Char -> Format.fprintf ppf "char"
     | String -> Format.fprintf ppf "string")
  | TVar x -> Format.fprintf ppf "%s" @@ Char.escaped (Char.chr (x + 97))  (** 97 is an ASCII code of letter "a" *)
  | TArr (l, r) ->
    (match l, r with
     | TArr _, _ -> Format.fprintf ppf "(%a) -> %a" pp_type l pp_type r
     | _, _ -> Format.fprintf ppf "%a -> %a" pp_type l pp_type r)
  | TList x ->
    (match x with
     | TArr _ -> Format.fprintf ppf "(%a) list" pp_type x
     | _ -> Format.fprintf ppf "%a list" pp_type x)
  | TTuple tup_list ->
    Format.fprintf
      ppf
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun _ _ -> Format.fprintf ppf " * ")
         (fun ppf typ -> pp_type ppf typ))
      tup_list
;;

let pp_error ppf (err : error) =
  match err with
  | `OccursCheck -> Format.fprintf ppf "Occurs check failed"
  | `UndeclaredVariable s -> Format.fprintf ppf "Undefined variable: %s" s
  | `NoConstructor s -> Format.fprintf ppf "Undefined constructor: %s" s
  | `UnificationFailed (l, r) ->
    Format.fprintf ppf "Unification failed on %a and %a" pp_type l pp_type r
  | `AddLater -> Format.fprintf ppf "Add later"
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;

let print_result expr =
  match run_inference expr with
  | Ok typ -> print_typ typ
  | Error x -> print_type_error x
;;
