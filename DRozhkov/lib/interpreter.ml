(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

module type MONAD = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module type MONADERROR = sig
  include MONAD

  val fail : 'e -> ('a, 'e) t
end

module MONAD_ADAPTER = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VList of value list
  | VFun of id * expression * bundle

and bundle = (id, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value fmt = function
  | VInt x -> fprintf fmt "%d" x
  | VBool x -> fprintf fmt "%b" x
  | VUnit -> fprintf fmt "()"
  | VList vs ->
    fprintf fmt "[%a]" (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "; ") pp_value) vs
  | VFun _ -> fprintf fmt "<fun>"
;;

module EnvValues = struct
  open Base

  type 'a t

  let empty = Map.empty (module String)
  let extend env (id, value) = Map.set env ~key:id ~data:value
  let update mp key data = Map.update mp key ~f:(function _ -> data)
  let singleton (id, value) = extend empty (id, value)
end

module Interpret (M : MONADERROR) = struct
  type error =
    | Division_by_zero
    | Let_bundle
    | Not_implemented
    | Pattern_matching_error
    | Unbound_value of id
    | Incorrect_type of value

  let pp_error fmt = function
    | Division_by_zero -> fprintf fmt "Division by zero"
    | Unbound_value id -> fprintf fmt "Unbound value %s" id
    | Incorrect_type v ->
      fprintf fmt "Value %a has incorrect type in expression" pp_value v
    | Let_bundle -> fprintf fmt "Let without in is not allowed in this part of expression"
    | Not_implemented ->
      Stdlib.print_endline "The expression is not implemented in the expression"
    | Pattern_matching_error ->
      fprintf fmt "Value can't be match with any case in this expression"
  ;;

  let match_pattern env = function
    | PAny, _ -> Some env
    | PConst (Int x), VInt v when x = v -> Some env
    | PConst (Bool x), VBool v when x = v -> Some env
    | PConst Empty, VUnit -> Some env
    | PVar id, v -> Some (EnvValues.singleton (id, v))
    | _ -> None
  ;;

  open M

  let lookup_env env id =
    match Base.Map.find env id with
    | None -> fail (Unbound_value id)
    | Some v -> return v
  ;;

  let eval =
    let rec helper env = function
      | Const c ->
        (match c with
         | Int x -> return (VInt x)
         | Bool x -> return (VBool x)
         | Empty -> return VUnit)
      | Var id ->
        let* v = lookup_env env id in
        return v
      | List es ->
        let* vs =
          Base.List.fold_right es ~init:(return []) ~f:(fun e acc ->
            let* v = helper env e in
            let* acc = acc in
            return (v :: acc))
        in
        return (VList vs)
      | IfThenElse (c, t, f) ->
        let* cv = helper env c in
        (match cv with
         | VBool true ->
           let* tv = helper env t in
           return tv
         | VBool false ->
           let* fv = helper env f in
           return fv
         | _ -> fail (Incorrect_type cv))
      | Fun (id, e) -> return (VFun (id, e, env))
      | Binop (op, l, r) ->
        let* lv = helper env l in
        let* rv = helper env r in
        (match op, lv, rv with
         | Add, VInt l, VInt r -> return (VInt (l + r))
         | Sub, VInt l, VInt r -> return (VInt (l - r))
         | Eq, VInt l, VInt r -> return (VBool (l = r))
         | Eq, VBool l, VBool r -> return (VBool (l <> r))
         | Neq, VInt l, VInt r -> return (VBool (l = r))
         | Neq, VBool l, VBool r -> return (VBool (l <> r))
         | Les, VInt l, VInt r -> return (VBool (l < r))
         | Leq, VInt l, VInt r -> return (VBool (l <= r))
         | Gre, VInt l, VInt r -> return (VBool (l > r))
         | Geq, VInt l, VInt r -> return (VBool (l >= r))
         | Mul, VInt l, VInt r -> return (VInt (l * r))
         | Div, VInt _, VInt 0 -> fail Division_by_zero
         | Div, VInt l, VInt r -> return (VInt (l / r))
         | _ -> fail (Incorrect_type rv))
      | Match (e, list) ->
        let* v = helper env e in
        eval_match env v list
      | App (f, e) ->
        let* fv = helper env f in
        let* ev = helper env e in
        (match fv with
         | VFun (id, e, fenv) ->
           let fenv = EnvValues.extend fenv (id, ev) in
           let updated_env =
             Base.Map.fold fenv ~init:env ~f:(fun ~key ~data acc_env ->
               EnvValues.extend acc_env (key, data))
           in
           let* v = helper updated_env e in
           return v
         | _ -> fail (Incorrect_type fv))
      | Let (_, name, e1, maybe_e2) ->
        (match maybe_e2 with
         | None -> fail Let_bundle
         | Some e2 ->
           let* v1 = helper env e1 in
           let env = EnvValues.extend env (name, v1) in
           let* v2 = helper env e2 in
           return v2)
    and eval_match env v = function
      | [] -> fail Pattern_matching_error
      | (pat, expr) :: tl ->
        let new_env = match_pattern env (pat, v) in
        (match new_env with
         | Some env -> helper env expr
         | None -> eval_match env v tl)
    in
    helper
  ;;

  let interpret p =
    let eval_let env = function
      | Let (_, id, e, None) ->
        let* v = eval env e in
        let env = EnvValues.update env id v in
        return (env, v)
      | expression ->
        let* v = eval env expression in
        return (env, v)
    in
    let* env, rs =
      Base.List.fold_left
        p
        ~init:(return (EnvValues.empty, []))
        ~f:(fun acc e ->
          let* env, rs = acc in
          let* env, res = eval_let env e in
          return (env, res :: rs))
    in
    return (env, List.rev rs)
  ;;
end

module Interpreter = Interpret (MONAD_ADAPTER)
