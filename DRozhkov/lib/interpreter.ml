(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

module type MONAD_ERROR = sig
  type ('a, 'e) t

  val fail : 'e -> ('a, 'e) t
  val return : 'a -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VList of value list
  | VFun of id * expression * bundle

and bundle = (id, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value fmt = function
  | VInt x -> fprintf fmt "%d " x
  | VBool x -> fprintf fmt "%b " x
  | VUnit -> fprintf fmt "()"
  | VList vs ->
    fprintf fmt "[%a]" (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "; ") pp_value) vs
  | VFun _ -> fprintf fmt "<fun> \n"
;;

type error =
  | Division_by_zero
  | Let_bundle
  | Pattern_matching_error
  | Unbound_value of id
  | Incorrect_type of value

let pp_error fmt = function
  | Division_by_zero -> fprintf fmt "Division by zero"
  | Unbound_value id -> fprintf fmt "Unbound value %s" id
  | Incorrect_type v -> fprintf fmt "Value %a has incorrect type in expression" pp_value v
  | Let_bundle -> fprintf fmt "Let without in is not allowed in this part of expression"
  | Pattern_matching_error ->
    fprintf fmt "Value can't be match with any case in this expression"
;;

module Env = struct
  open Base

  let empty = Map.empty (module String)
end

module Interpret (M : MONAD_ERROR) = struct
  open M

  let lookup_env env id =
    match Base.Map.find env id with
    | None -> fail (Unbound_value id)
    | Some v -> return v
  ;;

  let binop (op, lv, rv) =
    match op, lv, rv with
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
    | _ -> fail (Incorrect_type rv)
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
        binop (op, lv, rv)
      | Match (e, cases) ->
        let* v = helper env e in
        let rec match_cases env v = function
          | (pat, expression) :: tl ->
            (match pat, v with
             | PAny, _ -> helper env expression
             | PVar id, v' ->
               let env' = Base.Map.set env ~key:id ~data:v' in
               helper env' expression
             | PConst const, cvalue ->
               (match const, cvalue with
                | Bool e1, VBool e2 when e1 = e2 -> helper env expression
                | Int e1, VInt e2 when e1 = e2 -> helper env expression
                | Empty, VUnit -> helper env expression
                | _ -> match_cases env v tl))
          | [] -> fail Pattern_matching_error
        in
        match_cases env v cases
      | App (f, e) ->
        let* fv = helper env f in
        let* ev = helper env e in
        (match fv with
         | VFun (id, e, fenv) ->
           let fenv = Base.Map.set fenv ~key:id ~data:ev in
           let updated_env =
             Base.Map.fold fenv ~init:env ~f:(fun ~key ~data acc_env ->
               Base.Map.set acc_env ~key ~data)
           in
           let* v = helper updated_env e in
           return v
         | _ -> fail (Incorrect_type fv))
      | Let (_, name, e1, maybe_e2) ->
        (match maybe_e2 with
         | None -> fail Let_bundle
         | Some e2 ->
           let* v1 = helper env e1 in
           let env = Base.Map.set env ~key:name ~data:v1 in
           let* v2 = helper env e2 in
           return v2)
    in
    helper
  ;;

  let interpret p =
    let eval_let env = function
      | Let (_, id, e, None) ->
        let* v = eval env e in
        let env' = Base.Map.update env id ~f:(function _ -> v) in
        return (env', v)
      | expression ->
        let* v = eval env expression in
        return (env, v)
    in
    let* env, rs =
      Base.List.fold_left
        p
        ~init:(return (Env.empty, []))
        ~f:(fun acc e ->
          let* env, rs = acc in
          let* env, res = eval_let env e in
          return (env, res :: rs))
    in
    return (env, List.rev rs)
  ;;
end

module MONAD_ADAPTER = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end

module Interpreter = Interpret (MONAD_ADAPTER)
