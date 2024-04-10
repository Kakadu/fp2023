(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Errorinter

(** main program *)

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | VTuple of value list
  | VFun of pattern * expr * (name * value) list
  | VLetWAPat of name * value
  | VLetAPat of name list * value
  | VCases of name
  | VNil (** [] *)
[@@deriving show { with_path = false }]

module type MonadFail = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

let is_constr = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

type environment = (name, value, String.comparator_witness) Map.t

module Environment (M : MonadFail) = struct
  open M

  let empty = Map.empty (module Base.String)

  let find_val map key =
    if String.length key = 0
    then fail (StringOfLengthZero key)
    else (
      match Map.find map key with
      | Some value -> return value
      | None when is_constr @@ String.get key 0 -> fail (UnboundConstructor key)
      | _ -> fail (UnboundValue key))
  ;;

  let add_bind map key value = Map.update map key ~f:(fun _ -> value)

  let add_binds map binds =
    List.fold ~f:(fun map (k, v) -> add_bind map k v) ~init:map binds
  ;;
end

module Interpret (M : MonadFail) = struct
  open M
  open Environment (M)

  let rec bind_fun_params ?(env = empty) =
    let bind_pat_list patl argl =
      let binded_list =
        List.fold2
          patl
          argl
          ~f:(fun acc pat arg ->
            let* acc = acc in
            let* binding = bind_fun_params ~env (pat, arg) in
            return (acc @ binding))
          ~init:(return [])
      in
      match binded_list with
      | Ok v -> v
      | _ -> fail MatchFailure
    in
    function
    | Wild, _ -> return []
    | Const c, app_arg ->
      (match c, app_arg with
       | CBool b1, VBool b2 when Bool.equal b1 b2 -> return []
       | CInt i1, VInt i2 when i1 = i2 -> return []
       | CString s1, VString s2 when String.equal s1 s2 -> return []
       | CNil, VList v ->
         (match v with
          | [] -> return []
          | _ -> fail MatchFailure)
       | _ -> fail Unreachable)
    | Var var, app_arg -> return [ var, app_arg ]
    | (PCon _ as pl), VList vl ->
      (match pl, vl with
       | PCon (h, t), hd :: tl ->
         let* evaledhd = bind_fun_params (h, hd) in
         let* evaledtl = bind_fun_params (t, VList tl) in
         return @@ evaledhd @ evaledtl
       | _ -> fail MatchFailure)
    | Tuple pl, VTuple vl -> bind_pat_list pl vl
    | Case (acase_id, _), value_to_match ->
      let* apat = find_val env acase_id in
      (match apat with
       | VLetAPat (_, VFun (apat_arg, apat_expr, apat_env)) ->
         let* bind_matching_val = bind_fun_params ~env (apat_arg, value_to_match) in
         let* eval_res_apat =
           eval apat_expr (add_binds (add_binds empty apat_env) bind_matching_val)
         in
         (match eval_res_apat with
          | VCases a when String.( = ) a acase_id -> return []
          | VInt _ | VString _ | VList _ | VTuple _ | VBool _ ->
            (match apat_arg with
             | Var name -> return [ name, eval_res_apat ]
             | _ -> fail Unreachable)
          | _ -> fail MatchFailure)
       | _ -> fail MatchFailure)
    | _ -> fail MatchFailure

  and eval_binding bind env =
    match bind with
    | Let (is_rec, name, body) ->
      if is_rec
      then
        let* func_body = eval body env in
        return @@ VLetWAPat (name, func_body)
      else eval body env
    | LetAct (act_name, body) ->
      let* fun_pat = eval body env in
      return @@ VLetAPat (act_name, fun_pat)
    | Expression expr -> eval expr env

  and eval expr env =
    match expr with
    | ConstExpr v ->
      (match v with
       | CBool b -> return @@ VBool b
       | CInt i -> return @@ VInt i
       | CString s -> return @@ VString s
       | CNil -> return VNil)
    | BinExpr (op, l, r) ->
      let* rigth_val = eval r env in
      let* left_val = eval l env in
      (match op, left_val, rigth_val with
       | Div, VInt _, VInt 0 -> fail DivisionByZero
       | Mod, VInt _, VInt 0 -> fail DivisionByZero
       | Add, VInt l, VInt r -> return @@ VInt (l + r)
       | Sub, VInt l, VInt r -> return @@ VInt (l - r)
       | Mul, VInt l, VInt r -> return @@ VInt (l * r)
       | Div, VInt l, VInt r -> return @@ VInt (l / r)
       | Mod, VInt l, VInt r -> return @@ VInt (l % r)
       | Less, VInt l, VInt r -> return @@ VBool (l < r)
       | LEq, VInt l, VInt r -> return @@ VBool (l <= r)
       | Gre, VInt l, VInt r -> return @@ VBool (l > r)
       | GEq, VInt l, VInt r -> return @@ VBool (l >= r)
       | Eq, VInt l, VInt r -> return @@ VBool (l = r)
       | NEq, VInt l, VInt r -> return @@ VBool (l <> r)
       | Con, h, VList tl -> return @@ VList (h :: tl)
       | Con, h, VNil -> return @@ VList (h :: [])
       | _ -> fail TypeError)
    | VarExpr id -> find_val env id
    | ListExpr (h, t) ->
      let* evaled = eval h env in
      let rec helper acc expr =
        match expr with
        | ConstExpr CNil -> acc
        | ListExpr (hd, tl) ->
          let* acc = acc in
          let* evaled = eval hd env in
          helper (return (evaled :: acc)) tl
        | _ ->
          let* acc = acc in
          let* evaled = eval expr env in
          return (evaled :: acc)
      in
      let* res = helper (return [ evaled ]) t in
      let res = VList (List.rev res) in
      return res
    | TupleExpr t ->
      let* eval_list = all (List.map t ~f:(fun expr -> eval expr env)) in
      return @@ VTuple eval_list
    | IfExpr (cond, e_then, e_else) ->
      let* cond_branch = eval cond env in
      (match cond_branch with
       | VBool b -> eval (if b then e_then else e_else) env
       | _ -> fail TypeError)
    | FunExpr (pat, expr) -> return @@ VFun (pat, expr, Map.to_alist env)
    | AppExpr (func, arg) ->
      let* fun_to_apply = eval func env in
      let* evaled_arg = eval arg env in
      (match fun_to_apply with
       | VFun (pat, expr, fun_env) ->
         let* res = bind_fun_params ~env (pat, evaled_arg) in
         eval expr (add_binds (add_binds empty fun_env) res)
       | VLetWAPat (name, VFun (pat, expr, fun_env)) ->
         let* res = bind_fun_params ~env (pat, evaled_arg) in
         eval
           expr
           (add_binds
              (add_bind
                 (add_binds empty fun_env)
                 name
                 (VLetWAPat (name, VFun (pat, expr, fun_env))))
              res)
       | _ -> fail TypeError)
    | MatchExpr (expr_match, cases) ->
      let* val_match = eval expr_match env in
      let rec eval_match = function
        | (pat, expr) :: cases ->
          run
            (bind_fun_params ~env (pat, val_match))
            ~ok:(fun binds -> eval expr (add_binds env binds))
            ~err:(fun _ -> eval_match cases)
        | [] -> fail MatchFailure
      in
      eval_match cases
    | CaseExpr constr_id -> return @@ VCases constr_id
    | LetInExpr (_, name, expr1, expr2) ->
      let* v_let = eval expr1 env in
      let env = add_bind env name v_let in
      eval expr2 env
  ;;

  let eval_program (program : struct_inter list) : (value, error_inter) t =
    let rec helper env = function
      | h :: [] -> eval_binding h env
      | [] -> fail EmptyProgram
      | h :: tl ->
        let* eval_h = eval_binding h env in
        let eval_env =
          match h with
          | Let (_, f, _) -> add_bind env f eval_h
          | LetAct (fl, _) ->
            List.fold_right ~f:(fun h acc -> add_bind acc h eval_h) ~init:env fl
          | _ -> env
        in
        helper eval_env tl
    in
    helper empty program
  ;;
end

module InterpretResult = Interpret (struct
    include Result

    let run x ~ok ~err =
      match x with
      | Ok v -> ok v
      | Error e -> err e
    ;;

    let ( let* ) monad f = bind monad ~f
  end)

let eval_program = InterpretResult.eval_program
