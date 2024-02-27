(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | DivisionByZero
  | UnboundValue of string
  | TypeMismatch
  | NotImplemented
  | PatternMatchingFailed

(** different kinds of values that can exist in the interpreted language *)
type value =
  | VInt of int
  | VBool of bool
  | VChar of char
  | VString of string
  | VUnit
  | VNil
  | VTuple of value list
  | VList of value list
  | VFun of pattern * expr * binding

and binding = (id, value, Base.String.comparator_witness) Base.Map.t

module type Monad = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Env (M : Monad) = struct
  open M

  let empty = Base.Map.empty (module Base.String)
  let extend key value env = Base.Map.update env key ~f:(fun _ -> value)

  let find map key =
    match Base.Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundValue key)
  ;;
end

module Interpret (M : Monad) = struct
  open M
  open Env (M)

  let eval_const = function
    | Int i -> return (VInt i)
    | Bool b -> return (VBool b)
    | Char c -> return (VChar c)
    | String s -> return (VString s)
    | Unit -> return VUnit
  ;;

  let eval_binop = function
    | Plus, VInt i1, VInt i2 -> return (VInt (i1 + i2))
    | Minus, VInt i1, VInt i2 -> return (VInt (i1 - i2))
    | Mult, VInt i1, VInt i2 -> return (VInt (i1 * i2))
    | Div, VInt _, VInt 0 -> fail DivisionByZero
    | Div, VInt i1, VInt i2 -> return (VInt (i1 / i2))
    | Eq, VInt i1, VInt i2 -> return (VBool (i1 = i2))
    | Neq, VInt i1, VInt i2 -> return (VBool (i1 <> i2))
    | Ltq, VInt i1, VInt i2 -> return (VBool (i1 <= i2))
    | Gtq, VInt i1, VInt i2 -> return (VBool (i1 >= i2))
    | Gt, VInt i1, VInt i2 -> return (VBool (i1 > i2))
    | Lt, VInt i1, VInt i2 -> return (VBool (i1 < i2))
    | And, VBool b1, VBool b2 -> return (VBool (b1 && b2))
    | Or, VBool b1, VBool b2 -> return (VBool (b1 || b2))
    | _, _, _ -> fail TypeMismatch
  ;;

  (** returns the binding (env) where the expression will be evaluated *)
  let eval_pattern env = function
    | PAny, _ -> Some env
    | PConst (Int x), VInt v when x = v -> Some env
    | PConst (Bool x), VBool v when x = v -> Some env
    | PConst Unit, VUnit -> Some env
    | PVar id, v -> Some (extend id v env)
    | _ -> None
  ;;

  (** looking up the value of the variable in the current env *)
  let lookup env x =
    let* v = find env x in
    match v with
    | VFun (p, e, env) -> return (VFun (p, e, extend x v env))
    | _ -> return v
  ;;

  let eval_expr =
    let rec helper env = function
      | EConst e -> eval_const e
      | EVar e -> lookup env e
      | EBinOp (op, e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        eval_binop (op, v1, v2)
      | EIfThenElse (e1, e2, e3) ->
        let* v1 = helper env e1 in
        (match v1 with
         | VBool true -> helper env e2
         | VBool false -> helper env e3
         | _ -> fail TypeMismatch)
      | EFun (x, e) -> return (VFun (x, e, env))
      | EApp (e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        (match v1 with
         | VFun (x, e, env) ->
           (match eval_pattern env (x, v2) with
            | Some env -> helper env e
            | None -> fail PatternMatchingFailed)
         | _ -> fail TypeMismatch)
      | ELet ((NonRec, _, e1), EUnit) -> helper env e1
      | ELet ((Rec, _, e1), EUnit) -> helper env e1
      | ELet ((NonRec, x, e1), e2) ->
        let* v1 = helper env e1 in
        let env' = extend x v1 env in
        let* v2 = helper env' e2 in
        return v2
      | ELet ((Rec, x, e1), e2) ->
        let* v1 = helper env e1 in
        let v2 =
          match v1 with
          | VFun (x, e, _) -> VFun (x, e, env)
          | _ -> v1
        in
        let env' = extend x v2 env in
        helper env' e2
      | _ -> fail NotImplemented
    in
    helper
  ;;

  let interpret_expr expr = eval_expr empty expr
end

module R = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end

module InterpretResult = Interpret (R)

let run_expr_interpreter = InterpretResult.interpret_expr

(** module for test typing *)
module PP = struct
  open Format

  let pp_value ppf = function
    | VInt x -> fprintf ppf "%d" x
    | VBool x -> fprintf ppf "%b" x
    | VUnit -> fprintf ppf "()"
    | VNil -> fprintf ppf "[]"
    | VFun _ -> fprintf ppf "<fun>"
    | _ -> fprintf ppf "TODO"
  ;;

  let pp_error ppf : error -> unit = function
    | DivisionByZero -> fprintf ppf "Division by zero"
    | UnboundValue s -> fprintf ppf "Unbound value %s" s
    | NotImplemented -> fprintf ppf "Not implemented"
    | TypeMismatch -> fprintf ppf "Operator and operand type mismatch"
    | PatternMatchingFailed -> fprintf ppf "Mismatch between function and arguments"
  ;;

  let print_value = printf "%a" pp_value
  let print_error = printf "%a" pp_error
end
