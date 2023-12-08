(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Errorinter

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
  val eval_program : program -> (value, error) M.t
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
    | _, VCases _ -> fail MatchFailure
    | Wild, _ -> return []
    | Const c, app_arg ->
      (match c, app_arg with
       | CBool b1, VBool b2 when Bool.equal b1 b2 -> return []
       | CInt i1, VInt i2 when i1 = i2 -> return []
       | CString s1, VString s2 when String.equal s1 s2 -> return []
       | CNil, VNil -> return []
       | _ -> fail MatchFailure)
    | Var var, app_arg -> return [ var, app_arg ]
    | Tuple pt, VTuple vt -> bind_pat_list pt vt
    | List pl, VList vl -> bind_pat_list pl vl
    | Case (acase_id, acase_args), value_to_match ->
      (match acase_id, acase_args, value_to_match with
       | "Some", acase_arg :: [], VSome v -> bind_fun_params ~env (acase_arg, v)
       | "None", [], VNone -> return []
       | _, _, VCases _ -> fail MatchFailure
       | "None", [], _ | "Some", _ :: [], _ -> fail MatchFailure
       | _ ->
         let* apat = find_val env acase_id in
         (match apat with
          | VLetAPat (choice, VFun (apat_arg, apat_expr, apat_env)) ->
            (match choice with
             | SingleChoice (_, is_opt) ->
               let rec apply_fun_apat args func =
                 let rec convert_from_pat_to_val =
                   let convert_list_of_pat l =
                     let* eval_list =
                       all (List.map l ~f:(fun pat -> convert_from_pat_to_val pat))
                     in
                     return @@ VTuple eval_list
                   in
                   function
                   | Wild -> fail MatchFailure
                   | Case (id, pats) ->
                     (match id, pats with
                      | "None", [] -> return VNone
                      | "Some", arg :: [] ->
                        let* convert_arg = convert_from_pat_to_val arg in
                        (match convert_arg with
                         | VNone -> return VNone
                         | _ -> return @@ VSome convert_arg)
                      | _ -> fail MatchFailure)
                   | Const c -> eval (ConstExpr c) env
                   | Var id -> eval (VarExpr id) env
                   | Tuple tpat -> convert_list_of_pat tpat
                   | List lpat -> convert_list_of_pat lpat
                 in
                 let bind_result_apat res =
                   match func with
                   | VFun (apat_arg, apat_expr, apat_env) ->
                     let* bind_val_to_match =
                       bind_fun_params (apat_arg, value_to_match)
                     in
                     let* res_apat =
                       eval
                         apat_expr
                         (add_binds (add_binds empty apat_env) bind_val_to_match)
                     in
                     (match res_apat, is_opt, res with
                      | VCases (aconstr_id, None), false, Const CNil
                      | VSome (VCases (aconstr_id, None)), true, Const CNil
                        when String.equal aconstr_id acase_id -> return []
                      | VCases (aconstr_id, Some v), false, _
                      | VSome (VCases (aconstr_id, Some v)), true, _
                        when String.equal aconstr_id acase_id -> bind_fun_params (res, v)
                      | _, false, _ -> bind_fun_params (res, res_apat)
                      | VSome v, true, _ -> bind_fun_params (res, v)
                      | _ -> fail MatchFailure)
                   | _ -> fail MatchFailure
                 in
                 let apply_arg arg other_args =
                   match func with
                   | VFun (apat_arg, apat_expr, apat_env) ->
                     let* convert_arg = convert_from_pat_to_val arg in
                     let* bind_arg = bind_fun_params (apat_arg, convert_arg) in
                     let* eval_new_func_apat =
                       eval apat_expr (add_binds (add_binds empty apat_env) bind_arg)
                     in
                     apply_fun_apat other_args eval_new_func_apat
                   | _ -> fail MatchFailure
                 in
                 match args with
                 | [] -> bind_result_apat (Const CNil)
                 | h :: [] ->
                   run
                     (apply_arg h [])
                     ~ok:(fun res -> return res)
                     ~err:(fun _ -> bind_result_apat h)
                 | h :: tl -> apply_arg h tl
               in
               apply_fun_apat acase_args (VFun (apat_arg, apat_expr, apat_env))
             | MultipleChoice _ ->
               (match acase_args with
                | res when List.length res < 2 ->
                  let* bind_matching_val =
                    bind_fun_params ~env (apat_arg, value_to_match)
                  in
                  let* eval_res_apat =
                    eval
                      apat_expr
                      (add_binds (add_binds empty apat_env) bind_matching_val)
                  in
                  (match eval_res_apat with
                   | VCases (apat_id, opt_res) when String.equal apat_id acase_id ->
                     (match opt_res, res with
                      | Some v, res :: [] -> bind_fun_params (res, v)
                      | None, res :: [] -> bind_fun_params (res, VNone)
                      | None, _ -> return []
                      | _ -> fail MatchFailure)
                   | _ -> fail MatchFailure)
                | _ -> fail Unreachable))
          | _ -> fail Unreachable))
    | _ -> fail MatchFailure

  and eval expr env =
    match expr with
    | ConstExpr v -> return @@ inter_const v
    | BinExpr (op, l, r) -> inter_binary op l r env
    | VarExpr id -> find_val env id
    | ListExpr l -> inter_list l env
    | TupleExpr t -> inter_tuple t env
    | IfExpr (cond, e_then, e_else) -> inter_if cond e_then e_else env
    | FunExpr (pat, expr) -> return @@ VFun (pat, expr, Map.to_alist env)
    | AppExpr (func, arg) -> inter_app func arg env
    | LetExpr (bool, name, body) -> inter_let bool name body env
    | MatchExpr (expr_match, cases) -> inter_match expr_match cases env
    | CaseExpr (constr_id, args) -> inter_case constr_id args env

  and inter_const = function
    | CBool b -> VBool b
    | CInt i -> VInt i
    | CString s -> VString s
    | CNil -> VNone

  and inter_list l env =
    let* eval_list = all (List.map l ~f:(fun expr -> eval expr env)) in
    return @@ VList eval_list

  and inter_tuple t env =
    let* eval_list = all (List.map t ~f:(fun expr -> eval expr env)) in
    return @@ VTuple eval_list

  and inter_binary op l r env =
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

  and inter_if cond e_then e_else env =
    let* cond_branch = eval cond env in
    match cond_branch with
    | VBool b ->
      let e = if b then e_then else e_else in
      eval e env
    | _ -> fail TypeError

  and inter_let bool name body env =
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

  and inter_app func arg env =
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

  and inter_match expr_match cases env =
    let* val_match = eval expr_match env in
    let rec eval_match = function
      | (pat, expr) :: cases ->
        run
          (bind_fun_params ~env (pat, val_match))
          ~ok:(fun binds -> eval expr (add_binds env binds))
          ~err:(fun _ -> eval_match cases)
      | [] -> fail NotImplemented
    in
    eval_match cases

  and inter_case constr_id args env =
    match constr_id, args with
    | "Some", arg :: [] ->
      let* opt_val = eval arg env in
      return @@ VSome opt_val
    | "None", [] -> return VNone
    | _, [] -> return @@ VCases (constr_id, None)
    | _, arg :: [] ->
      let* arg_val = eval arg env in
      (match arg_val with
       | VCases _ -> fail TypeError
       | _ -> return @@ VCases (constr_id, Some arg_val))
    | _ -> fail TypeError
  ;;

  let eval_program (program : expr list) : (value, error) t =
    let rec helper env = function
      | h :: [] -> eval h env
      | [] -> fail EmptyProgram
      | h :: tl ->
        let* eval_h = eval h env in
        let eval_env =
          let rec eval_env_apat env = function
            | [] -> env
            | h :: tl -> add_bind (eval_env_apat env tl) h eval_h
          in
          match h with
          | (LetExpr (_, Name f, _) | LetExpr (_, ActivePaterns (SingleChoice (f, _)), _))
            when not @@ String.equal f "_" -> add_bind env f eval_h
          | LetExpr (_, ActivePaterns (MultipleChoice fl), _) -> eval_env_apat env fl
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
     | Error e -> Format.printf "(Error while interpreting): %a" pp_error e)
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

let%test _ =
  match eval_program test with
  | Ok (VInt 29) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* 5 = 5 *)

let test = [ BinExpr (Eq, ConstExpr (CInt 5), ConstExpr (CInt 5)) ]

let%test _ =
  match eval_program test with
  | Ok (VBool true) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let rec fact n = if n = 1 then 1 else n * (fact (n - 1))

   fact 5
*)

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
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let check input = match input with | 2 -> 5 | 5 -> 10

   check 5
*)
let test =
  [ LetExpr
      ( false
      , Name "check"
      , FunExpr
          ( Var "input"
          , MatchExpr
              ( VarExpr "input"
              , [ Const (CInt 2), ConstExpr (CInt 5)
                ; Const (CInt 5), ConstExpr (CInt 10)
                ] ) ) )
  ; AppExpr (VarExpr "check", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 10) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let (|Even|Odd|) v = if v % 2 = 0 then Even else Odd

   let myfunc v = match v with | Even -> 1 | Odd -> 5

   myfunc 5
*)

let test =
  [ LetExpr
      ( false
      , ActivePaterns (MultipleChoice [ "Even"; "Odd" ])
      , FunExpr
          ( Var "input"
          , IfExpr
              ( BinExpr
                  ( Eq
                  , BinExpr (Mod, VarExpr "input", ConstExpr (CInt 2))
                  , ConstExpr (CInt 0) )
              , CaseExpr ("Even", [])
              , CaseExpr ("Odd", []) ) ) )
  ; LetExpr
      ( false
      , Name "myfunc"
      , FunExpr
          ( Var "input"
          , MatchExpr
              ( VarExpr "input"
              , [ Case ("Even", []), ConstExpr (CInt 1)
                ; Case ("Odd", []), ConstExpr (CInt 5)
                ] ) ) )
  ; AppExpr (VarExpr "myfunc", ConstExpr (CInt 6))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 1) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

let test =
  [ LetExpr
      ( false
      , ActivePaterns (SingleChoice ("Even", false))
      , FunExpr
          ( Var "v"
          , IfExpr
              ( BinExpr
                  (Eq, BinExpr (Mod, VarExpr "v", ConstExpr (CInt 2)), ConstExpr (CInt 0))
              , CaseExpr ("Even", [])
              , ConstExpr (CInt 0) ) ) )
  ; LetExpr
      ( false
      , Name "good"
      , FunExpr
          ( Var "input"
          , MatchExpr
              ( VarExpr "input"
              , [ Case ("Even", []), ConstExpr (CInt 5); Wild, ConstExpr (CInt 7) ] ) ) )
  ; AppExpr (VarExpr "good", ConstExpr (CInt 101))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 7) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;
