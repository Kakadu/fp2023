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
  | IncorrectType of value
  | NotAFunction of value
  | LetWithoutIn (** Inner let expressions without in are forbidden *)
  | NoMatchCase of value
  | NotImplemented

let pp_error fmt =
  let open Format in
  function
  | DivisionByZero -> fprintf fmt "Division by zero error"
  | UnboundValue id -> fprintf fmt "Unbound value %s" id
  | IncorrectType v -> fprintf fmt "Value %a has incorrect type in expression" pp_value v
  | NotAFunction v ->
    fprintf fmt "Value %a is not a function; it can not be applied" pp_value v
  | LetWithoutIn -> fprintf fmt "Let without in is not allowed in this part of expression"
  | NoMatchCase v ->
    fprintf fmt "Value %a can not be match with any case in this expression" pp_value v
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

  let singleton (id, value) = extend empty (id, value)
end

module Interpret (M : MONAD_FAIL) = struct
  module PatternMatching : sig
    type ('a, 'err) t =
      | Matched of 'a
      | NotMatched
      | Failed of 'err

    val match_pattern : pattern -> value -> (bindings, error) t
  end = struct
    type ('a, 'err) t =
      | Matched of 'a
      | NotMatched
      | Failed of 'err

    let matched a = Matched a
    let not_matched = NotMatched
    let fail err = Failed err

    let ( >>= ) a f =
      match a with
      | Matched v -> f v
      | NotMatched -> not_matched
      | Failed err -> fail err
    ;;

    let ( let* ) = ( >>= )

    let match_pattern : pattern -> value -> (bindings, error) t =
      let rec helper pat value =
        match pat, value with
        | PWild, _ -> matched Env.empty
        | PEmpty, VList [] -> matched Env.empty
        | PEmpty, VList _ -> not_matched
        | PConst (CInt x), VInt v when x = v -> matched Env.empty
        | PConst (CInt _), VInt _ -> not_matched
        | PConst (CBool x), VBool v when x = v -> matched Env.empty
        | PConst (CBool _), VBool _ -> not_matched
        | PConst CUnit, VUnit -> matched Env.empty
        | PVar id, v ->
          let env = Env.singleton (id, v) in
          matched env
        | PTuple (p1, p2, ps), VTuple (v1, v2, vs) when List.compare_lengths ps vs = 0 ->
          let* env1 = helper p1 v1 in
          let* env2 = helper p2 v2 in
          let pvs = Base.List.zip_exn ps vs in
          let* env3 =
            Base.List.fold_left pvs ~init:(matched Env.empty) ~f:(fun acc (p, v) ->
              let* acc = acc in
              let* env = helper p v in
              let env = Env.add env acc in
              matched env)
          in
          let env = Env.add env3 (Env.add env2 env1) in
          matched env
        | PCons (p1, p2, ps), VList vs ->
          let ps, pl =
            match List.rev ps with
            | h :: tl -> p1 :: p2 :: List.rev tl, h
            | [] -> [ p1 ], p2
          in
          let* env, vs =
            Base.List.fold_left
              ps
              ~init:(matched (Env.empty, vs))
              ~f:(fun acc p ->
                let* acc, vs = acc in
                let* env, vs =
                  match vs with
                  | h :: tl ->
                    let* env = helper p h in
                    matched (env, tl)
                  | [] -> not_matched
                in
                matched (Env.add env acc, vs))
          in
          let* env1 = helper pl (VList vs) in
          let env = Env.add env1 env in
          matched env
        | POr (p1, p2, ps), v ->
          let* env =
            Base.List.fold_left (p1 :: p2 :: ps) ~init:not_matched ~f:(fun acc p ->
              match acc with
              | Matched env -> Matched env
              | Failed err -> Failed err
              | NotMatched ->
                let* env = helper p v in
                matched env)
          in
          matched env
        | _ -> fail (IncorrectType value)
      in
      helper
    ;;
  end

  open PatternMatching
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
         | _ -> fail (IncorrectType rv))
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
         | _ -> fail (IncorrectType cv))
      | EMatch (e, cases) ->
        let* v = helper env e in
        let* rv =
          Base.List.fold_left cases ~init:(return NotMatched) ~f:(fun acc (p, e) ->
            let* acc = acc in
            match acc, match_pattern p v with
            | Failed err, _ -> fail err
            | _, Failed err -> fail err
            | Matched _, _ -> return acc
            | NotMatched, Matched case_env ->
              let env = Env.add case_env env in
              let* v = helper env e in
              return (Matched v)
            | _ -> return NotMatched)
        in
        (match rv with
         | Matched v -> return v
         | NotMatched -> fail (NoMatchCase v)
         | Failed err -> fail err)
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
         | _ -> fail (NotAFunction fv))
    in
    helper
  ;;

  open Infer

  let interpret_single env (e, ty) =
    match e with
    | ELet (_, id, e, None) ->
      let* v = eval env e in
      let env = Env.extend env (id, v) in
      return (env, { id = (if id = "_" then None else Some id); value = v; ty })
    | e ->
      let* v = eval env e in
      return (env, { id = None; value = v; ty })
  ;;

  let interpret program =
    let* env, rs =
      Base.List.fold_left
        program
        ~init:(return (Env.empty, []))
        ~f:(fun acc e ->
          let* env, rs = acc in
          let* env, res = interpret_single env e in
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
