(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module type MonadFail = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | VTuple of value list
  | VLetWAPat of name * value (* without active patterns *)
  | VLetAPat of name list * value (* with active patterns *)
  | VFun of pattern * expr * (name * value) list
  | VCases of name * value
  | VSome of value
  | VNone
[@@deriving show]

let is_constr = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

type environment = (name, value, String.comparator_witness) Map.t

type interpret_error =
  | Division_by_zero
  | UnboundValue of string
  | UnboundConstructor of string
  | FunctionCompare
  | MatchFailure
  | EmptyProgram
  | TypeError
  | Unreachable
  | NotImplemented

let pp_interpret_error fmt = function
  | Division_by_zero -> Format.fprintf fmt "Exception: Division_by_zero."
  | UnboundValue s -> Format.fprintf fmt "Error: Unbound value %s" s
  | UnboundConstructor s -> Format.fprintf fmt "Error: Unbound constructor %s" s
  | FunctionCompare ->
    Format.fprintf fmt "Exception: Invalid_argument \"compare: functional value\""
  | MatchFailure ->
    Format.fprintf fmt "Exception: this pattern-matching is not exhaustive."
  | EmptyProgram -> Format.fprintf fmt "Error: the program was not provided or was empty"
  | TypeError -> Format.fprintf fmt "Error: type mismatch, a different type was expected"
  | Unreachable ->
    Format.fprintf fmt "Error: Unreachable error... Something went wrong..."
  | NotImplemented -> Format.fprintf fmt "This feature has not yet been implemented"
;;

module Environment (M : MonadFail) = struct
  open M

  let empty = Map.empty (module Base.String)

  let find_val map key =
    match Map.find map key with
    | Some value -> return value
    | None when is_constr @@ String.get key 0 -> fail (UnboundConstructor key)
    | _ -> fail (UnboundValue key)
  ;;

  let add_bind map key value = Map.update map key ~f:(fun _ -> value)

  let add_binds map binds =
    List.fold ~f:(fun map (k, v) -> add_bind map k v) ~init:map binds
  ;;
end

module Interpret (M : MonadFail) : sig
  val eval_program : program -> (value, interpret_error) M.t
end = struct
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
       | _ -> fail MatchFailure)
    | Var var, app_arg -> return [ var, app_arg ]
    | Tuple pt, VTuple vt -> bind_pat_list pt vt
    | List pl, VList vl -> bind_pat_list pl vl
    | _ -> fail MatchFailure
  ;;

  let inter_const = function
    | CBool b -> VBool b
    | CInt i -> VInt i
    | CString s -> VString s
  ;;

  let inter_list l eval env =
    let* eval_list = all (List.map l ~f:(fun expr -> eval expr env)) in
    return @@ VList eval_list
  ;;

  let inter_tuple t eval env =
    let* eval_list = all (List.map t ~f:(fun expr -> eval expr env)) in
    return @@ VTuple eval_list
  ;;

  let inter_binary op l r eval env =
    let* rigth_val = eval r env in
    let* left_val = eval l env in
    match op, left_val, rigth_val with
    | Div, VInt _l, VInt 0 -> fail Division_by_zero
    | Mod, VInt _l, VInt 0 -> fail Division_by_zero
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
    | _ -> fail Unreachable
  ;;

  let inter_if cond e_then e_else eval env =
    let* cond_branch = eval cond env in
    match cond_branch with
    | VBool b ->
      let e = if b then e_then else e_else in
      eval e env
    | _ -> fail TypeError
  ;;

  let inter_match expr_match cases eval env =
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
  ;;

  let inter_let bool name body eval env =
    match bool with
    | true ->
      (match name with
       | Name name ->
         let* func_body = eval body env in
         return @@ VLetWAPat (name, func_body)
       | _ -> fail Unreachable)
    | false ->
      (match name with
       | Name _ -> eval body env
       | ActivePaterns a_pat ->
         let* fun_pat = eval body env in
         return @@ VLetAPat (a_pat, fun_pat))
  ;;

  let inter_app func arg eval env =
    let* fun_to_apply = eval func env in
    match fun_to_apply with
    | VFun (pat, expr, fun_env) ->
      let* arg_to_apply = eval arg env in
      let* res = bind_fun_params ~env (pat, arg_to_apply) in
      eval expr (add_binds (add_binds empty fun_env) res)
    | VLetWAPat (name, VFun (pat, expr, fun_env)) ->
      let* arg_to_apply = eval arg env in
      let* res = bind_fun_params ~env (pat, arg_to_apply) in
      eval
        expr
        (add_binds
           (add_bind
              (add_binds empty fun_env)
              name
              (VLetWAPat (name, VFun (pat, expr, fun_env))))
           res)
    | _ -> fail TypeError
  ;;

  let rec eval expr env =
    match expr with
    | ConstExpr v -> return @@ inter_const v
    | BinExpr (op, l, r) -> inter_binary op l r eval env
    | VarExpr id -> find_val env id
    | ListExpr l -> inter_list l eval env
    | TupleExpr t -> inter_tuple t eval env
    | IfExpr (cond, e_then, e_else) -> inter_if cond e_then e_else eval env
    | FunExpr (pat, expr) -> return @@ VFun (pat, expr, Map.to_alist env)
    | AppExpr (func, arg) -> inter_app func arg eval env
    | LetExpr (bool, name, body) -> inter_let bool name body eval env
    | MatchExpr (expr_match, cases) -> inter_match expr_match cases eval env
    | CaseExpr (constr_id, args) ->
      (match constr_id, args with
       | "Some", arg :: [] ->
         let* opt_val = eval arg env in
         return @@ VSome opt_val
       | "None", [] -> return VNone
       | _, [] -> return @@ VCases (constr_id, VNone)
       | _, arg :: [] ->
         let* arg_val = eval arg env in
         (match arg_val with
          | VCases _ -> fail TypeError
          | _ -> return @@ VCases (constr_id, VSome arg_val))
       | _ -> fail TypeError)
  ;;

  let eval_program (program : expr list) : (value, interpret_error) t =
    let rec helper env = function
      | h :: [] -> eval h env
      | [] -> fail EmptyProgram
      | h :: tl ->
        let* eval_h = eval h env in
        let eval_env =
          match h with
          | LetExpr (_, Name f, _) when not @@ String.equal f "_" -> add_bind env f eval_h
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

let run input =
  match Parser.main_parse input with
  | Ok ast ->
    (match eval_program ast with
     | Ok res -> pp_value Format.std_formatter res
     | Error e -> Format.printf "(Error while interpreting): %a" pp_interpret_error e)
  | Error e -> Format.printf "(Error while parsing): %s" e
;;

(* TESTS INTERPRET *)

(*  1 + 2 + 3 + 4 * 5 + 6 / 3 + (10 - 5) / 5 *)
let test =
  [ BinExpr
      ( Add
      , BinExpr
          ( Add
          , BinExpr
              ( Add
              , BinExpr
                  ( Add
                  , BinExpr (Add, ConstExpr (CInt 1), ConstExpr (CInt 2))
                  , ConstExpr (CInt 3) )
              , BinExpr (Mul, ConstExpr (CInt 4), ConstExpr (CInt 5)) )
          , BinExpr (Div, ConstExpr (CInt 6), ConstExpr (CInt 3)) )
      , BinExpr
          (Div, BinExpr (Sub, ConstExpr (CInt 10), ConstExpr (CInt 5)), ConstExpr (CInt 5))
      )
  ]
;;

let test =
  [ BinExpr
      ( Mul
      , BinExpr
          ( Mul
          , BinExpr (Add, ConstExpr (CInt 1), ConstExpr (CInt 1))
          , BinExpr (Sub, ConstExpr (CInt 5), ConstExpr (CInt 2)) )
      , BinExpr (Div, ConstExpr (CInt 42), ConstExpr (CInt 6)) )
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 42) -> true
  | _ -> false
;;

let test =
  [ LetExpr (false, Name "f", FunExpr (Var "x", BinExpr (Add, VarExpr "x", VarExpr "x")))
  ; AppExpr (VarExpr "f", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 10) -> true
  | _ -> false
;;

let test =
  [ LetExpr
      ( true
      , Name "fact"
      , FunExpr
          ( Var "n"
          , IfExpr
              ( BinExpr (Eq, VarExpr "n", ConstExpr (CInt 1))
              , ConstExpr (CInt 1)
              , BinExpr
                  ( Mul
                  , VarExpr "n"
                  , AppExpr
                      (VarExpr "fact", BinExpr (Sub, VarExpr "n", ConstExpr (CInt 1))) )
              ) ) )
  ; AppExpr (VarExpr "fact", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 120) -> true
  | _ -> false
;;

let test =
  [ LetExpr
      ( false
      , Name "recognize"
      , FunExpr
          ( Var "input"
          , MatchExpr
              ( VarExpr "input"
              , [ Const (CInt 1), ConstExpr (CInt 5); Wild, ConstExpr (CInt 20) ] ) ) )
  ; AppExpr (VarExpr "recognize", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 20) -> true
  | _ -> false
;;
