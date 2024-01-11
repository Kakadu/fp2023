(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MONAD_FAIL = sig
  type ('a, 'err) t

  val return : 'a -> ('a, 'err) t
  val fail : 'err -> ('a, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

  module Syntax : sig
    val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  end
end

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VTuple of value * value * value list
  | VList of value list
  | VFun of id * expr * bindings

and bindings = (id, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value fmt =
  let open Format in
  function
  | VInt x -> fprintf fmt "%d" x
  | VBool x -> fprintf fmt "%b" x
  | VUnit -> fprintf fmt "()"
  | VTuple (v1, v2, vs) ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt ", ") pp_value)
      (v1 :: v2 :: vs)
  | VList vs ->
    fprintf fmt "[%a]" (pp_print_list ~pp_sep:(fun fmt _ -> fprintf fmt "; ") pp_value) vs
  | VFun _ -> fprintf fmt "<fun>"
;;

type interpret_result =
  { id : id option
  ; value : value
  ; ty : Typing.ty
  }

let pp_interpret_result fmt res =
  let open Format in
  match res.id with
  | None -> fprintf fmt "- : %a = %a" Typing.pp_ty res.ty pp_value res.value
  | Some id -> fprintf fmt "val %s : %a = %a" id Typing.pp_ty res.ty pp_value res.value
;;

type error =
  | DivisionByZero
  | UnboundValue of id
  | IncorrectType
  | LetWithoutIn (** Inner let expressions without in are forbidden *)
  | TypeInferFailed of Typing.error
  | NotImplemented

let pp_error fmt =
  let open Format in
  function
  | DivisionByZero -> fprintf fmt "Division by zero error"
  | UnboundValue id -> fprintf fmt "Unbound value %s" id
  | IncorrectType -> fprintf fmt "Type mismatch"
  | LetWithoutIn -> fprintf fmt "Let without in is not allowed in this part of expression"
  | TypeInferFailed err -> fprintf fmt "%a" Typing.pp_error err
  | NotImplemented -> Stdlib.print_endline "Expression contains not implemented features"
;;

module Env = struct
  type t = bindings

  let empty = Base.Map.empty (module Base.String)

  let extend : t -> id * value -> t =
    fun env (id, value) -> Base.Map.set env ~key:id ~data:value
  ;;

  (* Add subenv's bindings to env without rewriting variables with the same names *)
  let add : t -> t -> t =
    fun env subenv ->
    Base.Map.fold env ~init:subenv ~f:(fun ~key ~data env -> extend env (key, data))
  ;;
end

module Interpret (M : MONAD_FAIL) = struct
  open M
  open M.Syntax

  let lookup_env env id =
    match Base.Map.find env id with
    | None -> fail (UnboundValue id)
    | Some v -> return v
  ;;

  let eval : Env.t -> expr -> (value, error) t =
    let rec helper env = function
      | EConst c ->
        (match c with
         | CInt x -> return (VInt x)
         | CBool x -> return (VBool x)
         | CUnit -> return VUnit)
      | EVar id ->
        let* v = lookup_env env id in
        return v
      | EBinop (op, l, r) ->
        let* lv = helper env l in
        let* rv = helper env r in
        (match op, lv, rv with
         | Add, VInt l, VInt r -> return (VInt (l + r))
         | Sub, VInt l, VInt r -> return (VInt (l - r))
         | Mul, VInt l, VInt r -> return (VInt (l * r))
         | Div, VInt _, VInt 0 -> fail DivisionByZero
         | Div, VInt l, VInt r -> return (VInt (l / r))
         | Eq, VInt l, VInt r -> return (VBool (l = r))
         | Eq, VBool l, VBool r -> return (VBool (l <> r))
         | Neq, VInt l, VInt r -> return (VBool (l = r))
         | Neq, VBool l, VBool r -> return (VBool (l <> r))
         | Les, VInt l, VInt r -> return (VBool (l < r))
         | Leq, VInt l, VInt r -> return (VBool (l <= r))
         | Gre, VInt l, VInt r -> return (VBool (l > r))
         | Geq, VInt l, VInt r -> return (VBool (l >= r))
         | _ -> fail IncorrectType)
      | ETuple (e1, e2, es) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        let* vs =
          Base.List.fold_right es ~init:(return []) ~f:(fun e acc ->
            let* v = helper env e in
            let* acc = acc in
            return (v :: acc))
        in
        return (VTuple (v1, v2, vs))
      | EList es ->
        let* vs =
          Base.List.fold_right es ~init:(return []) ~f:(fun e acc ->
            let* v = helper env e in
            let* acc = acc in
            return (v :: acc))
        in
        return (VList vs)
      | EBranch (c, t, f) ->
        let* cv = helper env c in
        (match cv with
         | VBool true ->
           let* tv = helper env t in
           return tv
         | VBool false ->
           let* fv = helper env f in
           return fv
         | _ -> fail IncorrectType)
      | EMatch _ -> fail NotImplemented
      | ELet (_, _, _, None) -> fail LetWithoutIn
      | ELet (_, id, e1, Some e2) ->
        let* v1 = helper env e1 in
        let env = Env.extend env (id, v1) in
        let* v2 = helper env e2 in
        return v2
      | EFun (id, e) -> return (VFun (id, e, env))
      | EApp (f, e) ->
        let* fv = helper env f in
        let* ev = helper env e in
        (match fv with
         | VFun (id, e, fenv) ->
           let fenv = Env.extend fenv (id, ev) in
           let fenv = Env.add fenv env in
           let* v = helper fenv e in
           return v
         | _ -> fail IncorrectType)
    in
    helper
  ;;

  open Infer

  let interpret_expr ?(tyenv = TypeEnv.empty) env e =
    let* ty =
      match run_infer ~env:tyenv e with
      | Result.Ok ty -> return ty
      | Result.Error err -> fail (TypeInferFailed err)
    in
    match e with
    | ELet (_, id, e, None) ->
      let* v = eval env e in
      let env = Env.extend env (id, v) in
      return (env, { id = Some id; value = v; ty })
    | e ->
      let* v = eval env e in
      return (env, { id = None; value = v; ty })
  ;;

  let interpret ?(tyenv = TypeEnv.empty) program =
    let* env, rs =
      Base.List.fold_left
        program
        ~init:(return (Env.empty, []))
        ~f:(fun acc e ->
          let* env, rs = acc in
          let* env, res = interpret_expr ~tyenv env e in
          return (env, res :: rs))
    in
    return (env, List.rev rs)
  ;;
end

module Eval : MONAD_FAIL with type ('a, 'err) t = ('a, 'err) Result.t = struct
  type ('a, 'err) t = ('a, 'err) Result.t

  let return a = Result.Ok a
  let fail err = Result.Error err

  let ( >>= ) a f =
    match a with
    | Result.Ok v -> f v
    | Result.Error err -> fail err
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

module Interpreter = Interpret (Eval)

let interpret = Interpreter.interpret
