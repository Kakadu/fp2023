(** Copyright 2021-2023, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | DivisionByZero
  | UnboundValue of string
  | EmptyInput
  | TypeMismatch
  | NotImplemented 

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
  let extend id value env = Base.Map.add env ~key:id ~data:value
  let update map key data = Base.Map.update map key ~f:(function _ -> data)

  let find map key =
    match Base.Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundValue key)
  ;;
end

module Interpret (M : Monad) = struct
  open M
  open Env (M)

  let match_pattern env = function
    | PAny, _ -> Some env
    | PConst (Int x), VInt v when x = v -> Some env
    | PConst (Bool x), VBool v when x = v -> Some env
    | PConst Unit, VUnit -> Some env
    | PVar id, v -> Some (extend id v empty)
    | _ -> None
  ;;

  let eval_binop op l r =
    match op, l, r with
    | Plus, VInt i1, VInt i2 -> return (VInt (i1 + i2))
    | Minus, VInt i1, VInt i2 -> return (VInt (i1 - i2))
    | Mult, VInt i1, VInt i2 -> return (VInt (i1 * i2))
    | Div, VInt i1, VInt i2 ->
      if i2 = 0 then fail DivisionByZero else return (VInt (i1 / i2))
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

  let eval_expr =
    let rec helper env = function
      | EConst e ->
        (match e with
         | Int i -> return (VInt i)
         | Bool b -> return (VBool b)
         | Char c -> return (VChar c)
         | String s -> return (VString s)
         | Unit -> return VUnit)
      | EVar name -> find env name
      | EBinOp (op, e1, e2) ->
        let* l = helper env e1 in
        let* r = helper env e2 in
        eval_binop op l r
      | EIfThenElse (e1, e2, e3) ->
        let* e = helper env e1 in
        (match e with
         | VBool true -> helper env e2
         | VBool false -> helper env e3
         | _ -> fail (TypeMismatch))
      | EFun (id, e) -> return (VFun (id, e, env))
      | ELet ((NonRec, _, e1), EUnit) -> helper env e1
        (* let env = update env name v1 in *)
      | _ -> fail NotImplemented
    in
    helper

    let eval_let_without_in env = function
      | LetDecl (NonRec, name, expr) ->
           let* v = eval_expr env expr in
           let env = update env name v in
           return (env, v)
      | LetDecl (Rec, name, expr) ->
        let* v = eval_expr env expr in
           let env = update env name v in
           return (env, v)
    ;;

  (** runs a sequence of expr (let bindings) in an environment *)
  (** update env after each expression is evaluated *)
  (* let eval_program prog =
    List.fold_left
      (fun acc toplevel ->
        let* acc = acc in
        let* env, _ = eval_let_without_in acc toplevel in
        return env)
      (return empty)
      prog *)
  ;;

  let interpret_expr expr = eval_expr empty expr 
  ;;
end

module R = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end

module InterpretResult = Interpret (R)

let run_expr_interpreter = InterpretResult.interpret_expr

(* let run = InterpretResult.eval_program *)

module PP = struct
  open Format
  let pp_value ppf =
    function
    | VInt x -> fprintf ppf "%d" x
    | VBool x -> fprintf ppf "%b" x
    | VChar c -> Format.fprintf ppf {|%C|} c
    | VString s -> Format.fprintf ppf {|%S|} s
    | VUnit -> fprintf ppf "()"
    | VNil -> fprintf ppf "[]"
    | VFun _ -> fprintf ppf "<fun>"
    | _ -> fprintf ppf "TODO"
  ;;

  let pp_error ppf : error -> unit =
    function
    | DivisionByZero -> fprintf ppf "Division by zero"
    | UnboundValue s -> fprintf ppf "Unbound value %s" s
    | EmptyInput -> fprintf ppf "Empty input to interpret"
    | NotImplemented -> fprintf ppf "Not implemented" 
    | TypeMismatch -> fprintf ppf "Operator and operand type mismatch"
  ;;

  let print_value = printf "%a" pp_value
  let print_error = printf "%a" pp_error
end
